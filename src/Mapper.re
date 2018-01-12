
open Spider_monkey_ast;

type mapper = {
  expression: (mapper, Expression.t) => Expression.t,
  pattern: (mapper, Pattern.t) => Pattern.t,
  statement: (mapper, Statement.t) => Statement.t
};

let mapMaybe = (mapper, item) => (switch item {
| None => None
| Some(item) => Some(mapper(item))
});

let mapLocPair = (mapper, (loc, item)) => (loc, mapper(item));

let mapSpread = (mapper, item) => Expression.(switch item {
| Spread((loc, {argument})) => Spread((loc, {argument: mapper.expression(mapper, argument)}))
| Expression(expression) => Expression(mapper.expression(mapper, expression))
});

let mapProperty = (mapper, item) => Expression.Object.(switch item {
| Property((loc, {key: Computed(key), value} as prop)) => Property((loc, {...prop, key: Computed(mapper.expression(mapper, key)), value: mapper.expression(mapper, value)}))
| Property((loc, {value} as prop)) => Property((loc, {...prop, value: mapper.expression(mapper, value)}))
| SpreadProperty((loc, {argument})) => SpreadProperty((loc, {argument: mapper.expression(mapper, argument)}))
});

let mapComprehensionBlock = (mapper, {Expression.Comprehension.Block.left, right, each}) => ({
  Expression.Comprehension.Block.left: mapper.pattern(mapper, left),
  right: mapper.expression(mapper, right),
  each
});

let rec mapJsxElement = (mapper, {JSX.openingElement: (loc, openingElement), closingElement, children}) => {
  JSX.openingElement: (loc, {...openingElement, JSX.Opening.attributes: List.map(mapJsxAttribute(mapper), openingElement.attributes)}),
  closingElement,
  children: List.map(mapLocPair(mapJsxChild(mapper)), children)
} and mapJsxChild = (mapper, child) => JSX.(switch child {
| Element(element) => Element(mapJsxElement(mapper, element))
| ExpressionContainer({expression: Expression(expression)}) => ExpressionContainer({expression: Expression(mapper.expression(mapper, expression))})
| ExpressionContainer(_) => child
| Text(_) => child
}) and mapJsxAttribute = (mapper, attribute) => JSX.Attribute.(switch attribute {
| Attribute((loc, {name, value: Some(ExpressionContainer(loc2, {expression: Expression(expression)}))})) =>
  Attribute((loc, {name, value: Some(ExpressionContainer(loc2, {expression: Expression(mapper.expression(mapper, expression))}))}))
| Attribute(_) => attribute
| SpreadAttribute((loc, {argument})) => SpreadAttribute((loc, {argument: mapper.expression(mapper, argument)}))
});

let mapBlock = (mapper, {Statement.Block.body}) => {
  Statement.Block.body: List.map(mapper.statement(mapper), body)
};

let mapCase = (mapper, {Statement.Switch.Case.test, consequent}) => {
  Statement.Switch.Case.test: mapMaybe(mapper.expression(mapper), test),
  consequent: List.map(mapper.statement(mapper), consequent)
};

let mapCatchClause = (mapper, {Statement.Try.CatchClause.param, body: (loc, block)}) => {
  Statement.Try.CatchClause.param: mapper.pattern(mapper, param),
  body: (loc, mapBlock(mapper, block))
};

let mapFunction = (mapper, {Function.params: (patterns, maybeRest), body} as fn) => ({
  ...fn,
  params: (List.map(mapper.pattern(mapper), patterns), maybeRest),
  body: switch body {
  | BodyBlock((loc, block)) => BodyBlock((loc, mapBlock(mapper, block)))
  | BodyExpression(expr) => BodyExpression(mapper.expression(mapper, expr))
  }
});

let mapClassBody = (mapper, element) => Class.Body.(switch element {
| Method((loc, {Class.Method.key, value: (loc2, fn), decorators} as m)) => Method((loc, {
    ...m,
    key: Expression.Object.Property.(switch key {
    | Literal(_)
    | Identifier(_) => key
    | Computed(key) => Computed(mapper.expression(mapper, key))
    }),
    value: (loc2, mapFunction(mapper, fn)),
    decorators: List.map(mapper.expression(mapper), decorators)
  }))
| Property((loc, {key, value} as p)) => Property((loc, {
    ...p,
    key: Expression.Object.Property.(switch key {
    | Literal(_)
    | Identifier(_) => key
    | Computed(key) => Computed(mapper.expression(mapper, key))
    }),
    value: mapMaybe(mapper.expression(mapper), value)
  }))
});

let mapVar = (mapper, {Statement.VariableDeclaration.declarations, kind}) => Statement.VariableDeclaration.{
  declarations: List.map(mapLocPair(({Declarator.id, init}) => {Declarator.id: mapper.pattern(mapper, id), init: mapMaybe(mapper.expression(mapper), init)}), declarations),
  kind
};

let mapInit = (mapper, init) => Statement.For.(switch init {
| InitDeclaration((loc, var)) => InitDeclaration((loc, mapVar(mapper, var)))
| InitExpression(expr) => InitExpression(mapper.expression(mapper, expr))
});

let mapCls = (mapper, {Class.body: (loc, {body: elements}), superClass, classDecorators} as cls) => {
  ...cls,
  superClass: mapMaybe(mapper.expression(mapper), superClass),
  classDecorators: List.map(mapper.expression(mapper), classDecorators),
  body: (loc, {body: List.map(mapClassBody(mapper), elements)})
};

let defaultMapper = {
  pattern: (mapper, (loc, pattern)) => Pattern.((loc, switch pattern {
  | Identifier(_) => pattern
  | Object({properties, typeAnnotation}) => Object({
    typeAnnotation,
    properties: List.map(prop => switch prop {
    | Object.Property((loc, {key, pattern, shorthand})) => Object.Property((loc, {
      key: switch key {
      | Computed(e) => Computed(mapper.expression(mapper, e))
      | _ => key
      },
      pattern: mapper.pattern(mapper, pattern),
      shorthand
    }))
    | RestProperty((loc, {argument})) => RestProperty((loc, {argument: mapper.pattern(mapper, argument)}))
    }, properties)
  })
  | Array({elements, typeAnnotation}) => Array({elements: List.map(
    mapMaybe(el => switch el {
    | Array.Element(p) => Array.Element(mapper.pattern(mapper, p))
    | RestElement((loc, {argument})) => RestElement((loc, {argument: mapper.pattern(mapper, argument)}))
    }),
    elements), typeAnnotation})
  | Assignment({left, right}) => Assignment({left: mapper.pattern(mapper, left), right: mapper.expression(mapper, right)})
  | Expression(expr) => Expression(mapper.expression(mapper, expr))
  })),

  expression: (mapper, (loc, expression): Spider_monkey_ast.Expression.t) => Expression.((loc, switch expression {
  | This
  | Identifier(_)
  | Literal(_)
  | MetaProperty(_)
  | Super => expression

  | Array({elements}) => Array({elements: List.map(mapMaybe(mapSpread(mapper)), elements)})
  | Object({properties}) => Object({properties: List.map(mapProperty(mapper), properties)})
  | Sequence({expressions}) => Sequence({expressions: List.map(mapper.expression(mapper), expressions)})
  | Unary({operator, prefix, argument}) => Unary({operator, prefix, argument: mapper.expression(mapper, argument)})
  | Binary({operator, left, right}) => Binary({operator, left: mapper.expression(mapper, left), right: mapper.expression(mapper, right)})
  | Assignment({operator, left, right}) => Assignment({operator, left: mapper.pattern(mapper, left), right: mapper.expression(mapper, right)})
  | Update({operator, argument, prefix}) => Update({operator, prefix, argument: mapper.expression(mapper, argument)})
  | Logical({operator, left, right}) => Logical({operator, left: mapper.expression(mapper, left), right: mapper.expression(mapper, right)})
  | Conditional({test, consequent, alternate}) => Conditional({test: mapper.expression(mapper, test), consequent: mapper.expression(mapper, consequent), alternate: mapper.expression(mapper, alternate)})
  | New({callee, arguments}) => New({callee: mapper.expression(mapper, callee), arguments: List.map(mapSpread(mapper), arguments)})
  | Call({callee, arguments}) => Call({callee: mapper.expression(mapper, callee), arguments: List.map(mapSpread(mapper), arguments)})
  | Member({_object, property: PropertyExpression(expression), computed}) => Member({computed, _object: mapper.expression(mapper, _object), property: PropertyExpression(mapper.expression(mapper, expression))})
  | Member({_object} as obj) => Member({...obj, _object: mapper.expression(mapper, _object)})
  | Yield({argument: Some(argument), delegate}) => Yield({delegate, argument: Some(mapper.expression(mapper, argument))})
  | Yield(_) => expression
  | Comprehension({blocks, filter}) => Comprehension({filter: mapMaybe(mapper.expression(mapper), filter), blocks: List.map(mapLocPair(mapComprehensionBlock(mapper)), blocks)})
  | Generator({blocks, filter}) => Generator({filter: mapMaybe(mapper.expression(mapper), filter), blocks: List.map(mapLocPair(mapComprehensionBlock(mapper)), blocks)})
  | TemplateLiteral({quasis, expressions}) => TemplateLiteral({quasis, expressions: List.map(mapper.expression(mapper), expressions)})
  | TaggedTemplate({tag, quasi: (loc, {quasis, expressions})}) => TaggedTemplate({tag, quasi: (loc, {quasis, expressions: List.map(mapper.expression(mapper), expressions)})})
  | JSXElement(element) => JSXElement(mapJsxElement(mapper, element))
  | TypeCast({expression, typeAnnotation}) => TypeCast({expression: mapper.expression(mapper, expression), typeAnnotation})

  | Function(fn) => Function(mapFunction(mapper, fn))
  | ArrowFunction(fn) => ArrowFunction(mapFunction(mapper, fn))
  | Class(cls) => Class(mapCls(mapper, cls))
  })),

  statement: (mapper, (loc, statement): Statement.t) => Statement.((loc, switch statement {
  | Debugger
  | ImportDeclaration(_)
  | Break(_)
  | Continue(_)
  | TypeAlias(_)
  | DeclareVariable(_)
  | DeclareFunction(_)
  | DeclareClass(_)
  | DeclareModule(_)
  | DeclareModuleExports(_)
  | DeclareExportDeclaration(_)
  | InterfaceDeclaration(_)
  | Empty => statement
  | Block(block) => Block(mapBlock(mapper, block))
  | Expression({expression}) => Expression({expression: mapper.expression(mapper, expression)})
  | If({test, consequent, alternate}) => If({test: mapper.expression(mapper, test), consequent: mapper.statement(mapper, consequent), alternate: mapMaybe(mapper.statement(mapper), alternate)})
  | Labeled({label, body}) => Labeled({label, body: mapper.statement(mapper, body)})
  | With({_object, body}) => With({_object: mapper.expression(mapper, _object), body: mapper.statement(mapper, body)})
  | Switch({discriminant, cases}) => Switch({discriminant: mapper.expression(mapper, discriminant), cases: List.map(mapLocPair(mapCase(mapper)), cases)})
  | Return({argument}) => Return({argument: mapMaybe(mapper.expression(mapper), argument)})
  | Throw({argument}) => Throw({argument: mapper.expression(mapper, argument)})
  | Try({block: (loc, block), handler, finalizer}) => Try({block: (loc, mapBlock(mapper, block)), handler: mapMaybe(mapLocPair(mapCatchClause(mapper)), handler), finalizer: mapMaybe(mapLocPair(mapBlock(mapper)), finalizer)})
  | While({test, body}) => While({test: mapper.expression(mapper, test), body: mapper.statement(mapper, body)})
  | DoWhile({body, test}) => DoWhile({test: mapper.expression(mapper, test), body: mapper.statement(mapper, body)})
  | For({init, test, update, body}) => For({init: mapMaybe(mapInit(mapper), init), test: mapMaybe(mapper.expression(mapper), test), update: mapMaybe(mapper.expression(mapper), update), body: mapper.statement(mapper, body)})
  | ForIn({left, right, body, each}) => ForIn({
    left: switch left {
    | LeftDeclaration((loc, var)) => LeftDeclaration((loc, mapVar(mapper, var)))
    | LeftExpression(expr) => LeftExpression(mapper.expression(mapper, expr))
    },
    right: mapper.expression(mapper, right),
    body: mapper.statement(mapper, body),
    each
  })
  | ForOf({left, right, body, async}) => ForOf({
    left: switch left {
    | LeftDeclaration((loc, var)) => LeftDeclaration((loc, mapVar(mapper, var)))
    | LeftExpression(expr) => LeftExpression(mapper.expression(mapper, expr))
    },
    right: mapper.expression(mapper, right),
    body: mapper.statement(mapper, body),
    async
  })

  | FunctionDeclaration(fn) => FunctionDeclaration(mapFunction(mapper, fn))
  | VariableDeclaration(var) => VariableDeclaration(mapVar(mapper, var))
  | ClassDeclaration(cls) => ClassDeclaration(mapCls(mapper, cls))
  | ExportNamedDeclaration({declaration} as decl) => ExportNamedDeclaration({
    ...decl,
    declaration: mapMaybe(mapper.statement(mapper), declaration)
  })
  | ExportDefaultDeclaration({declaration: Declaration(st), exportKind}) => ExportDefaultDeclaration({declaration: Declaration(mapper.statement(mapper, st)), exportKind})
  | ExportDefaultDeclaration({declaration: Expression(e), exportKind}) => ExportDefaultDeclaration({declaration: Expression(mapper.expression(mapper, e)), exportKind})
  }))
};
