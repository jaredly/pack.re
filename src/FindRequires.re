
let parseContents = (fileName, contents) => {
  let parse_options =
    Some(
      Parser_env.{
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

  /* TODO do some DCE based on process.env.NODE_ENV == "production" */

  let mapper = Spider_monkey_ast.{
    ...Mapper.defaultMapper,
    expression: (mapper, (loc, st)) => Expression.(switch st {
    | Identifier((loc, name)) => {
      /* print_endline(name); */
      (loc, st)
    }
    | Call({callee: (_, Identifier((_, "require"))), arguments: [Expression((_, Literal({value: String(path), raw})))]}) => {
      requires := [Types.{pos: Loc.(loc.start.offset), length: Loc.(loc._end.offset - loc.start.offset), text: path}, ...requires^];
      (loc, st)
    }
    | _ => Mapper.defaultMapper.expression(mapper, (loc, st))
    })
  };

  List.map(mapper.statement(mapper), statements) |> ignore;
  requires^ |> List.sort((a, b) => a.Types.pos - b.Types.pos)
};


