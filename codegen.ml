exception Error of string

let context = Llvm.global_context ()
let my_module = Llvm.create_module context "my jit"
let builder = Llvm.builder context
let named_values: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let double_type: Llvm.lltype = Llvm.double_type context

let rec codegen_expr (ex : Ast.expr) : Llvm.llvalue =
  match ex with
  | Ast.Number n -> Llvm.const_float double_type n
  | Ast.Variable name ->
      begin
      try Hashtbl.find named_values name
      with Not_found -> raise (Error ("unknown variable name " ^ name))
      end
  | Ast.If (cond, expTrue, expFalse) ->
      let cond_val = codegen_expr cond in
      let zero = Llvm.const_float double_type 0.0 in
      (* ordered (neighter arg NAN), not-equal comparison *)
      let comparison = Llvm.build_fcmp Llvm.Fcmp.One cond_val zero "ifcond" builder in
      
      (* get the block that we inserted the comparison into, aka current block *)
      let start_bb = Llvm.insertion_block builder in
      (* get ref to function we are in *)
      let current_func = Llvm.block_parent start_bb in

      (* emit 'then' value *)
      let then_bb = Llvm.append_block context "then" current_func in
      Llvm.position_at_end then_bb builder;
      let then_val = codegen_expr expTrue in
      (* generation of the 'then' could have changed current block, get new block *)
      let new_then_bb = Llvm.insertion_block builder in

      (* emit 'else' value *)
      let else_bb = Llvm.append_block context "else" current_func in
      Llvm.position_at_end else_bb builder;
      let else_val = codegen_expr expFalse in
      let new_else_bb = Llvm.insertion_block builder in (* see comment above *)

      (* emit merge block *)
      let merge_bb = Llvm.append_block context "ifcont" current_func in
      Llvm.position_at_end merge_bb builder;
      let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
      let phi = Llvm.build_phi incoming "iftmp" builder in

      (* return to start block to add the conditional branch to other blocks *)
      Llvm.position_at_end start_bb builder;
      let _ = Llvm.build_cond_br comparison then_bb else_bb builder in

      (* branch from ends of 'then' and 'else' blocks to 'merge' block *)
      Llvm.position_at_end new_then_bb builder;
      let _ = Llvm.build_br merge_bb builder in
      Llvm.position_at_end new_else_bb builder;
      let _ = Llvm.build_br merge_bb builder in

      (* position the builder at the end of the merge block, where construction will continue *)
      Llvm.position_at_end merge_bb builder;

      phi
  | Ast.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin match op with
      | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
      | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
      | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
      | '<' -> 
          (* Convert the bool (1 or 0) to double (1.0 or 0.0) *)
          let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
          Llvm.build_uitofp i double_type "booltmp" builder
      | _ -> raise (Error ("invalid binary operator " ^ Batteries.String.of_char op))
      end
  | Ast.Call (func_name, args) ->
      (* lookup the function in the module table *)
      let callee : Llvm.llvalue = 
        begin 
        match Llvm.lookup_function func_name my_module with
        | Some callee -> callee
        | None -> raise (Error ("unknown function referenced: " ^ func_name))
        end
      in
      (* get the parameters of the function we want to call *)
      let params : Llvm.llvalue array = Llvm.params callee in

      (* check that num args is same as num params *)
      if Array.length params = Array.length args then () else
        raise (Error "incorrect # of arguments passed");

      (* codegen each argument into an llvalue *)
      let args : Llvm.llvalue array = Array.map codegen_expr args in
      Llvm.build_call callee args "calltmp" builder

let codegen_proto (proto : Ast.proto) : Llvm.llvalue =
  match proto with
  | Ast.Prototype (name, args) ->
      (* the type of the function is double -> double -> ... -> double *)
      let doubles = Array.make (Array.length args) double_type in
      let ft = Llvm.function_type double_type doubles in
      let f =
        match Llvm.lookup_function name my_module with
        | None -> Llvm.declare_function name ft my_module
        (* there is already a function called 'name' *)
        | Some f ->
            (* if f already has a body, there is an error *)
            if Array.length (Llvm.basic_blocks f) == 0 then () else
              raise (Error "redefinition of a function");

            (* if f doesn't already have a body, it is a forwards declaration or extern *)
            (* ensure that our definition matches the extern one *)
            if Array.length (Llvm.params f) = Array.length args then () else
              raise (Error "redefinition of a function with differen # of args");
            f
      in
      (* set names for all of the function arguments *)
      Array.iteri (fun i arg_llvalue ->
        let arg_name : string = args.(i) in
        Llvm.set_value_name arg_name arg_llvalue;
        Hashtbl.add named_values arg_name arg_llvalue; (* register the names *)
      ) (Llvm.params f);
      f

let codegen_func (func : Ast.func) : Llvm.llvalue =
  match func with
  | Ast.Function (proto, body) ->
      (* clear the table, which has variable names from the last function call *)
      Hashtbl.clear named_values;
      let func_value : Llvm.llvalue = codegen_proto proto in
      let basic_block : Llvm.llbasicblock = Llvm.append_block context "entry" func_value in

      (* position builder at end of block, then add return value *)
      Llvm.position_at_end basic_block builder;
      try
        let ret_val = codegen_expr body in
        let _ = Llvm.build_ret ret_val builder in

        (* validate the generated code *)
        Llvm_analysis.assert_valid_function func_value;

        (* optimize the function *)
        (* TODO: no longer running the pass manager on a function by function basis *)
        (*let modified_module = Llvm.PassManager.run_function func_value fpm in *)
        (*if modified_module then () else print_endline "passes didn't modify module";*)

        func_value
      with e -> 
        Llvm.delete_function func_value;
        raise e







