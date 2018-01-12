
let parseContents = (fileName, contents) => {
  let parse_options =
    Some(
      Parser_env.{
        /***
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         */
        esproposal_class_instance_fields: true,
        esproposal_class_static_fields: true,
        esproposal_decorators: true,
        esproposal_export_star_as: true,
        types: true,
        use_strict: false
      }
    );

  let (ast, _errors) =
    Parser_flow.program_file(~fail=true, ~parse_options, contents, Some(Loc.SourceFile(fileName)));
  let (_loc, statements, _comments) = ast;

  let requires = ref([]);

  let mapper = Spider_monkey_ast.{
    ...Mapper.defaultMapper,
    expression: (mapper, (loc, st)) => Expression.(switch st {
    | Identifier((loc, name)) => {
      /* print_endline(name); */
      (loc, st)
    }
    | Call({callee: (_, Identifier((_, "require"))), arguments: [Expression((loc, Literal({value: String(path), raw})))]}) => {
      requires := [Types.{pos: Loc.(loc.start.offset), length: String.length(raw), text: path}, ...requires^];
      (loc, st)
    }
    | _ => Mapper.defaultMapper.expression(mapper, (loc, st))
    })
  };

  List.map(mapper.statement(mapper), statements) |> ignore;
  List.rev(requires^)
  /* output_string(stdout, Config.ast_impl_magic_number);
  output_value(stdout, file);
  let result: Parsetree.structure =
    statements |> List.map((statementWrap) => topStatementsMapper(statementWrap)) |> List.concat;
  output_value(stdout, result) */
};


