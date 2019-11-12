// Code generated; DO NOT EDIT.

#[derive(Clone, Debug, PartialEq)]
pub enum Tsx {
    End = 0,
    Identifier = 1,
    HashBangLine = 2,
    Export = 3,
    STAR = 4,
    Default = 5,
    EQ = 6,
    As = 7,
    Namespace = 8,
    LBRACE = 9,
    COMMA = 10,
    RBRACE = 11,
    Type = 12,
    Typeof = 13,
    Import = 14,
    From = 15,
    Var = 16,
    Let = 17,
    Const = 18,
    If = 19,
    Else = 20,
    Switch = 21,
    For = 22,
    LPAREN = 23,
    RPAREN = 24,
    Await = 25,
    In = 26,
    Of = 27,
    While = 28,
    Do = 29,
    Try = 30,
    With = 31,
    Break = 32,
    Continue = 33,
    Debugger = 34,
    Return = 35,
    Throw = 36,
    SEMI = 37,
    COLON = 38,
    Case = 39,
    Catch = 40,
    Finally = 41,
    Yield = 42,
    LBRACK = 43,
    RBRACK = 44,
    LT = 45,
    GT = 46,
    SLASH = 47,
    JsxText = 48,
    Identifier2 = 49,
    DOT = 50,
    Class = 51,
    Async = 52,
    Function = 53,
    EQGT = 54,
    New = 55,
    PLUSEQ = 56,
    DASHEQ = 57,
    STAREQ = 58,
    SLASHEQ = 59,
    PERCENTEQ = 60,
    CARETEQ = 61,
    AMPEQ = 62,
    PIPEEQ = 63,
    GTGTEQ = 64,
    GTGTGTEQ = 65,
    LTLTEQ = 66,
    STARSTAREQ = 67,
    DOTDOTDOT = 68,
    QMARK = 69,
    AMPAMP = 70,
    PIPEPIPE = 71,
    GTGT = 72,
    GTGTGT = 73,
    LTLT = 74,
    AMP = 75,
    CARET = 76,
    PIPE = 77,
    PLUS = 78,
    DASH = 79,
    PERCENT = 80,
    STARSTAR = 81,
    LTEQ = 82,
    EQEQ = 83,
    EQEQEQ = 84,
    BANGEQ = 85,
    BANGEQEQ = 86,
    GTEQ = 87,
    Instanceof = 88,
    BANG = 89,
    TILDE = 90,
    Void = 91,
    Delete = 92,
    PLUSPLUS = 93,
    DASHDASH = 94,
    DQUOTE = 95,
    StringToken1 = 96,
    SQUOTE = 97,
    StringToken2 = 98,
    EscapeSequence = 99,
    Comment = 100,
    BQUOTE = 101,
    DOLLARLBRACE = 102,
    SLASH2 = 103,
    RegexPattern = 104,
    RegexFlags = 105,
    Number = 106,
    Target = 107,
    This = 108,
    Super = 109,
    True = 110,
    False = 111,
    Null = 112,
    Undefined = 113,
    AT = 114,
    Static = 115,
    Abstract = 116,
    Get = 117,
    Set = 118,
    Declare = 119,
    Public = 120,
    Private = 121,
    Protected = 122,
    Module = 123,
    Any = 124,
    Number2 = 125,
    Boolean = 126,
    String = 127,
    Symbol = 128,
    Require = 129,
    Implements = 130,
    Global = 131,
    Interface = 132,
    Extends = 133,
    Enum = 134,
    Readonly = 135,
    Is = 136,
    Keyof = 137,
    LBRACEPIPE = 138,
    PIPERBRACE = 139,
    AutomaticSemicolon = 140,
    TemplateChars = 141,
    Program = 142,
    ExportStatement = 143,
    ExportClause = 144,
    ImportExportSpecifier = 145,
    Declaration = 146,
    Import2 = 147,
    ImportStatement = 148,
    ImportClause = 149,
    FromClause = 150,
    NamespaceImport = 151,
    NamedImports = 152,
    ExpressionStatement = 153,
    VariableDeclaration = 154,
    LexicalDeclaration = 155,
    VariableDeclarator = 156,
    StatementBlock = 157,
    IfStatement = 158,
    SwitchStatement = 159,
    ForStatement = 160,
    ForInStatement = 161,
    ForHeader = 162,
    WhileStatement = 163,
    DoStatement = 164,
    TryStatement = 165,
    WithStatement = 166,
    BreakStatement = 167,
    ContinueStatement = 168,
    DebuggerStatement = 169,
    ReturnStatement = 170,
    ThrowStatement = 171,
    EmptyStatement = 172,
    LabeledStatement = 173,
    SwitchBody = 174,
    SwitchCase = 175,
    SwitchDefault = 176,
    CatchClause = 177,
    FinallyClause = 178,
    ParenthesizedExpression = 179,
    Expression = 180,
    YieldExpression = 181,
    Object = 182,
    AssignmentPattern = 183,
    Array = 184,
    JsxElement = 185,
    JsxFragment = 186,
    JsxExpression = 187,
    JsxOpeningElement = 188,
    NestedIdentifier = 189,
    JsxNamespaceName = 190,
    JsxClosingElement = 191,
    JsxSelfClosingElement = 192,
    JsxAttribute = 193,
    Class2 = 194,
    ClassDeclaration = 195,
    ClassHeritage = 196,
    Function2 = 197,
    FunctionDeclaration = 198,
    GeneratorFunction = 199,
    GeneratorFunctionDeclaration = 200,
    ArrowFunction = 201,
    CallSignature = 202,
    CallExpression = 203,
    NewExpression = 204,
    AwaitExpression = 205,
    MemberExpression = 206,
    SubscriptExpression = 207,
    AssignmentExpression = 208,
    AugmentedAssignmentExpression = 209,
    Initializer = 210,
    SpreadElement = 211,
    TernaryExpression = 212,
    BinaryExpression = 213,
    UnaryExpression = 214,
    UpdateExpression = 215,
    SequenceExpression = 216,
    String2 = 217,
    TemplateString = 218,
    TemplateSubstitution = 219,
    Regex = 220,
    MetaProperty = 221,
    Arguments = 222,
    Decorator = 223,
    MemberExpression2 = 224,
    CallExpression2 = 225,
    ClassBody = 226,
    PublicFieldDefinition = 227,
    FormalParameters = 228,
    RestParameter = 229,
    MethodDefinition = 230,
    Pair = 231,
    PropertyName = 232,
    ComputedPropertyName = 233,
    NonNullExpression = 234,
    MethodSignature = 235,
    AbstractMethodSignature = 236,
    FunctionSignature = 237,
    AsExpression = 238,
    ImportRequireClause = 239,
    ImplementsClause = 240,
    AmbientDeclaration = 241,
    AbstractClassDeclaration = 242,
    Module2 = 243,
    InternalModule = 244,
    Module3 = 245,
    ImportAlias = 246,
    NestedTypeIdentifier = 247,
    InterfaceDeclaration = 248,
    ExtendsClause = 249,
    EnumDeclaration = 250,
    EnumBody = 251,
    EnumAssignment = 252,
    TypeAliasDeclaration = 253,
    AccessibilityModifier = 254,
    RequiredParameter = 255,
    OptionalParameter = 256,
    ParameterName = 257,
    TypeAnnotation = 258,
    Type2 = 259,
    ConstructorType = 260,
    PrimaryType = 261,
    GenericType = 262,
    TypePredicate = 263,
    TypeQuery = 264,
    IndexTypeQuery = 265,
    LookupType = 266,
    MappedTypeClause = 267,
    LiteralType = 268,
    UnaryExpression2 = 269,
    ExistentialType = 270,
    FlowMaybeType = 271,
    ParenthesizedType = 272,
    PredefinedType = 273,
    TypeArguments = 274,
    ObjectType = 275,
    CallSignature2 = 276,
    PropertySignature = 277,
    TypeParameters = 278,
    TypeParameter = 279,
    DefaultType = 280,
    Constraint = 281,
    ConstructSignature = 282,
    IndexSignature = 283,
    ArrayType = 284,
    TupleType = 285,
    UnionType = 286,
    IntersectionType = 287,
    FunctionType = 288,
    ProgramRepeat1 = 289,
    ExportStatementRepeat1 = 290,
    ExportClauseRepeat1 = 291,
    NamedImportsRepeat1 = 292,
    VariableDeclarationRepeat1 = 293,
    SwitchBodyRepeat1 = 294,
    ObjectRepeat1 = 295,
    ArrayRepeat1 = 296,
    JsxElementRepeat1 = 297,
    JsxOpeningElementRepeat1 = 298,
    StringRepeat1 = 299,
    StringRepeat2 = 300,
    TemplateStringRepeat1 = 301,
    ClassBodyRepeat1 = 302,
    FormalParametersRepeat1 = 303,
    ImplementsClauseRepeat1 = 304,
    ExtendsClauseRepeat1 = 305,
    EnumBodyRepeat1 = 306,
    ObjectTypeRepeat1 = 307,
    TypeParametersRepeat1 = 308,
    ArrayPattern = 309,
    ExportSpecifier = 310,
    ImportSpecifier = 311,
    ObjectPattern = 312,
    PropertyIdentifier = 313,
    ShorthandPropertyIdentifier = 314,
    StatementIdentifier = 315,
    TypeIdentifier = 316,
    Error = 317,
}

impl Into<&'static str> for Tsx {
    fn into(self) -> &'static str {
        match self {
            Tsx::End => "end",
            Tsx::Identifier => "identifier",
            Tsx::HashBangLine => "hash_bang_line",
            Tsx::Export => "export",
            Tsx::STAR => "*",
            Tsx::Default => "default",
            Tsx::EQ => "=",
            Tsx::As => "as",
            Tsx::Namespace => "namespace",
            Tsx::LBRACE => "{",
            Tsx::COMMA => ",",
            Tsx::RBRACE => "}",
            Tsx::Type => "type",
            Tsx::Typeof => "typeof",
            Tsx::Import => "import",
            Tsx::From => "from",
            Tsx::Var => "var",
            Tsx::Let => "let",
            Tsx::Const => "const",
            Tsx::If => "if",
            Tsx::Else => "else",
            Tsx::Switch => "switch",
            Tsx::For => "for",
            Tsx::LPAREN => "(",
            Tsx::RPAREN => ")",
            Tsx::Await => "await",
            Tsx::In => "in",
            Tsx::Of => "of",
            Tsx::While => "while",
            Tsx::Do => "do",
            Tsx::Try => "try",
            Tsx::With => "with",
            Tsx::Break => "break",
            Tsx::Continue => "continue",
            Tsx::Debugger => "debugger",
            Tsx::Return => "return",
            Tsx::Throw => "throw",
            Tsx::SEMI => ";",
            Tsx::COLON => ":",
            Tsx::Case => "case",
            Tsx::Catch => "catch",
            Tsx::Finally => "finally",
            Tsx::Yield => "yield",
            Tsx::LBRACK => "[",
            Tsx::RBRACK => "]",
            Tsx::LT => "<",
            Tsx::GT => ">",
            Tsx::SLASH => "/",
            Tsx::JsxText => "jsx_text",
            Tsx::Identifier2 => "identifier",
            Tsx::DOT => ".",
            Tsx::Class => "class",
            Tsx::Async => "async",
            Tsx::Function => "function",
            Tsx::EQGT => "=>",
            Tsx::New => "new",
            Tsx::PLUSEQ => "+=",
            Tsx::DASHEQ => "-=",
            Tsx::STAREQ => "*=",
            Tsx::SLASHEQ => "/=",
            Tsx::PERCENTEQ => "%=",
            Tsx::CARETEQ => "^=",
            Tsx::AMPEQ => "&=",
            Tsx::PIPEEQ => "|=",
            Tsx::GTGTEQ => ">>=",
            Tsx::GTGTGTEQ => ">>>=",
            Tsx::LTLTEQ => "<<=",
            Tsx::STARSTAREQ => "**=",
            Tsx::DOTDOTDOT => "...",
            Tsx::QMARK => "?",
            Tsx::AMPAMP => "&&",
            Tsx::PIPEPIPE => "||",
            Tsx::GTGT => ">>",
            Tsx::GTGTGT => ">>>",
            Tsx::LTLT => "<<",
            Tsx::AMP => "&",
            Tsx::CARET => "^",
            Tsx::PIPE => "|",
            Tsx::PLUS => "+",
            Tsx::DASH => "-",
            Tsx::PERCENT => "%",
            Tsx::STARSTAR => "**",
            Tsx::LTEQ => "<=",
            Tsx::EQEQ => "==",
            Tsx::EQEQEQ => "===",
            Tsx::BANGEQ => "!=",
            Tsx::BANGEQEQ => "!==",
            Tsx::GTEQ => ">=",
            Tsx::Instanceof => "instanceof",
            Tsx::BANG => "!",
            Tsx::TILDE => "~",
            Tsx::Void => "void",
            Tsx::Delete => "delete",
            Tsx::PLUSPLUS => "++",
            Tsx::DASHDASH => "--",
            Tsx::DQUOTE => "\"",
            Tsx::StringToken1 => "string_token1",
            Tsx::SQUOTE => "'",
            Tsx::StringToken2 => "string_token2",
            Tsx::EscapeSequence => "escape_sequence",
            Tsx::Comment => "comment",
            Tsx::BQUOTE => "`",
            Tsx::DOLLARLBRACE => "${",
            Tsx::SLASH2 => "/",
            Tsx::RegexPattern => "regex_pattern",
            Tsx::RegexFlags => "regex_flags",
            Tsx::Number => "number",
            Tsx::Target => "target",
            Tsx::This => "this",
            Tsx::Super => "super",
            Tsx::True => "true",
            Tsx::False => "false",
            Tsx::Null => "null",
            Tsx::Undefined => "undefined",
            Tsx::AT => "@",
            Tsx::Static => "static",
            Tsx::Abstract => "abstract",
            Tsx::Get => "get",
            Tsx::Set => "set",
            Tsx::Declare => "declare",
            Tsx::Public => "public",
            Tsx::Private => "private",
            Tsx::Protected => "protected",
            Tsx::Module => "module",
            Tsx::Any => "any",
            Tsx::Number2 => "number",
            Tsx::Boolean => "boolean",
            Tsx::String => "string",
            Tsx::Symbol => "symbol",
            Tsx::Require => "require",
            Tsx::Implements => "implements",
            Tsx::Global => "global",
            Tsx::Interface => "interface",
            Tsx::Extends => "extends",
            Tsx::Enum => "enum",
            Tsx::Readonly => "readonly",
            Tsx::Is => "is",
            Tsx::Keyof => "keyof",
            Tsx::LBRACEPIPE => "{|",
            Tsx::PIPERBRACE => "|}",
            Tsx::AutomaticSemicolon => "_automatic_semicolon",
            Tsx::TemplateChars => "_template_chars",
            Tsx::Program => "program",
            Tsx::ExportStatement => "export_statement",
            Tsx::ExportClause => "export_clause",
            Tsx::ImportExportSpecifier => "_import_export_specifier",
            Tsx::Declaration => "_declaration",
            Tsx::Import2 => "import",
            Tsx::ImportStatement => "import_statement",
            Tsx::ImportClause => "import_clause",
            Tsx::FromClause => "_from_clause",
            Tsx::NamespaceImport => "namespace_import",
            Tsx::NamedImports => "named_imports",
            Tsx::ExpressionStatement => "expression_statement",
            Tsx::VariableDeclaration => "variable_declaration",
            Tsx::LexicalDeclaration => "lexical_declaration",
            Tsx::VariableDeclarator => "variable_declarator",
            Tsx::StatementBlock => "statement_block",
            Tsx::IfStatement => "if_statement",
            Tsx::SwitchStatement => "switch_statement",
            Tsx::ForStatement => "for_statement",
            Tsx::ForInStatement => "for_in_statement",
            Tsx::ForHeader => "_for_header",
            Tsx::WhileStatement => "while_statement",
            Tsx::DoStatement => "do_statement",
            Tsx::TryStatement => "try_statement",
            Tsx::WithStatement => "with_statement",
            Tsx::BreakStatement => "break_statement",
            Tsx::ContinueStatement => "continue_statement",
            Tsx::DebuggerStatement => "debugger_statement",
            Tsx::ReturnStatement => "return_statement",
            Tsx::ThrowStatement => "throw_statement",
            Tsx::EmptyStatement => "empty_statement",
            Tsx::LabeledStatement => "labeled_statement",
            Tsx::SwitchBody => "switch_body",
            Tsx::SwitchCase => "switch_case",
            Tsx::SwitchDefault => "switch_default",
            Tsx::CatchClause => "catch_clause",
            Tsx::FinallyClause => "finally_clause",
            Tsx::ParenthesizedExpression => "parenthesized_expression",
            Tsx::Expression => "_expression",
            Tsx::YieldExpression => "yield_expression",
            Tsx::Object => "object",
            Tsx::AssignmentPattern => "assignment_pattern",
            Tsx::Array => "array",
            Tsx::JsxElement => "jsx_element",
            Tsx::JsxFragment => "jsx_fragment",
            Tsx::JsxExpression => "jsx_expression",
            Tsx::JsxOpeningElement => "jsx_opening_element",
            Tsx::NestedIdentifier => "nested_identifier",
            Tsx::JsxNamespaceName => "jsx_namespace_name",
            Tsx::JsxClosingElement => "jsx_closing_element",
            Tsx::JsxSelfClosingElement => "jsx_self_closing_element",
            Tsx::JsxAttribute => "jsx_attribute",
            Tsx::Class2 => "class",
            Tsx::ClassDeclaration => "class_declaration",
            Tsx::ClassHeritage => "class_heritage",
            Tsx::Function2 => "function",
            Tsx::FunctionDeclaration => "function_declaration",
            Tsx::GeneratorFunction => "generator_function",
            Tsx::GeneratorFunctionDeclaration => "generator_function_declaration",
            Tsx::ArrowFunction => "arrow_function",
            Tsx::CallSignature => "_call_signature",
            Tsx::CallExpression => "call_expression",
            Tsx::NewExpression => "new_expression",
            Tsx::AwaitExpression => "await_expression",
            Tsx::MemberExpression => "member_expression",
            Tsx::SubscriptExpression => "subscript_expression",
            Tsx::AssignmentExpression => "assignment_expression",
            Tsx::AugmentedAssignmentExpression => "augmented_assignment_expression",
            Tsx::Initializer => "_initializer",
            Tsx::SpreadElement => "spread_element",
            Tsx::TernaryExpression => "ternary_expression",
            Tsx::BinaryExpression => "binary_expression",
            Tsx::UnaryExpression => "unary_expression",
            Tsx::UpdateExpression => "update_expression",
            Tsx::SequenceExpression => "sequence_expression",
            Tsx::String2 => "string",
            Tsx::TemplateString => "template_string",
            Tsx::TemplateSubstitution => "template_substitution",
            Tsx::Regex => "regex",
            Tsx::MetaProperty => "meta_property",
            Tsx::Arguments => "arguments",
            Tsx::Decorator => "decorator",
            Tsx::MemberExpression2 => "member_expression",
            Tsx::CallExpression2 => "call_expression",
            Tsx::ClassBody => "class_body",
            Tsx::PublicFieldDefinition => "public_field_definition",
            Tsx::FormalParameters => "formal_parameters",
            Tsx::RestParameter => "rest_parameter",
            Tsx::MethodDefinition => "method_definition",
            Tsx::Pair => "pair",
            Tsx::PropertyName => "_property_name",
            Tsx::ComputedPropertyName => "computed_property_name",
            Tsx::NonNullExpression => "non_null_expression",
            Tsx::MethodSignature => "method_signature",
            Tsx::AbstractMethodSignature => "abstract_method_signature",
            Tsx::FunctionSignature => "function_signature",
            Tsx::AsExpression => "as_expression",
            Tsx::ImportRequireClause => "import_require_clause",
            Tsx::ImplementsClause => "implements_clause",
            Tsx::AmbientDeclaration => "ambient_declaration",
            Tsx::AbstractClassDeclaration => "abstract_class_declaration",
            Tsx::Module2 => "module",
            Tsx::InternalModule => "internal_module",
            Tsx::Module3 => "_module",
            Tsx::ImportAlias => "import_alias",
            Tsx::NestedTypeIdentifier => "nested_type_identifier",
            Tsx::InterfaceDeclaration => "interface_declaration",
            Tsx::ExtendsClause => "extends_clause",
            Tsx::EnumDeclaration => "enum_declaration",
            Tsx::EnumBody => "enum_body",
            Tsx::EnumAssignment => "enum_assignment",
            Tsx::TypeAliasDeclaration => "type_alias_declaration",
            Tsx::AccessibilityModifier => "accessibility_modifier",
            Tsx::RequiredParameter => "required_parameter",
            Tsx::OptionalParameter => "optional_parameter",
            Tsx::ParameterName => "_parameter_name",
            Tsx::TypeAnnotation => "type_annotation",
            Tsx::Type2 => "_type",
            Tsx::ConstructorType => "constructor_type",
            Tsx::PrimaryType => "_primary_type",
            Tsx::GenericType => "generic_type",
            Tsx::TypePredicate => "type_predicate",
            Tsx::TypeQuery => "type_query",
            Tsx::IndexTypeQuery => "index_type_query",
            Tsx::LookupType => "lookup_type",
            Tsx::MappedTypeClause => "mapped_type_clause",
            Tsx::LiteralType => "literal_type",
            Tsx::UnaryExpression2 => "unary_expression",
            Tsx::ExistentialType => "existential_type",
            Tsx::FlowMaybeType => "flow_maybe_type",
            Tsx::ParenthesizedType => "parenthesized_type",
            Tsx::PredefinedType => "predefined_type",
            Tsx::TypeArguments => "type_arguments",
            Tsx::ObjectType => "object_type",
            Tsx::CallSignature2 => "call_signature",
            Tsx::PropertySignature => "property_signature",
            Tsx::TypeParameters => "type_parameters",
            Tsx::TypeParameter => "type_parameter",
            Tsx::DefaultType => "default_type",
            Tsx::Constraint => "constraint",
            Tsx::ConstructSignature => "construct_signature",
            Tsx::IndexSignature => "index_signature",
            Tsx::ArrayType => "array_type",
            Tsx::TupleType => "tuple_type",
            Tsx::UnionType => "union_type",
            Tsx::IntersectionType => "intersection_type",
            Tsx::FunctionType => "function_type",
            Tsx::ProgramRepeat1 => "program_repeat1",
            Tsx::ExportStatementRepeat1 => "export_statement_repeat1",
            Tsx::ExportClauseRepeat1 => "export_clause_repeat1",
            Tsx::NamedImportsRepeat1 => "named_imports_repeat1",
            Tsx::VariableDeclarationRepeat1 => "variable_declaration_repeat1",
            Tsx::SwitchBodyRepeat1 => "switch_body_repeat1",
            Tsx::ObjectRepeat1 => "object_repeat1",
            Tsx::ArrayRepeat1 => "array_repeat1",
            Tsx::JsxElementRepeat1 => "jsx_element_repeat1",
            Tsx::JsxOpeningElementRepeat1 => "jsx_opening_element_repeat1",
            Tsx::StringRepeat1 => "string_repeat1",
            Tsx::StringRepeat2 => "string_repeat2",
            Tsx::TemplateStringRepeat1 => "template_string_repeat1",
            Tsx::ClassBodyRepeat1 => "class_body_repeat1",
            Tsx::FormalParametersRepeat1 => "formal_parameters_repeat1",
            Tsx::ImplementsClauseRepeat1 => "implements_clause_repeat1",
            Tsx::ExtendsClauseRepeat1 => "extends_clause_repeat1",
            Tsx::EnumBodyRepeat1 => "enum_body_repeat1",
            Tsx::ObjectTypeRepeat1 => "object_type_repeat1",
            Tsx::TypeParametersRepeat1 => "type_parameters_repeat1",
            Tsx::ArrayPattern => "array_pattern",
            Tsx::ExportSpecifier => "export_specifier",
            Tsx::ImportSpecifier => "import_specifier",
            Tsx::ObjectPattern => "object_pattern",
            Tsx::PropertyIdentifier => "property_identifier",
            Tsx::ShorthandPropertyIdentifier => "shorthand_property_identifier",
            Tsx::StatementIdentifier => "statement_identifier",
            Tsx::TypeIdentifier => "type_identifier",
            Tsx::Error => "ERROR",
        }
    }
}

#[allow(clippy::unreadable_literal)]
static KEYS: phf::Map<&'static str, Tsx> = ::phf::Map {
    key: 3347381344252206323,
    disps: ::phf::Slice::Static(&[
        (0, 233),
        (0, 11),
        (0, 154),
        (5, 292),
        (2, 102),
        (0, 0),
        (2, 132),
        (0, 8),
        (0, 154),
        (0, 3),
        (0, 49),
        (0, 109),
        (0, 41),
        (0, 79),
        (2, 79),
        (0, 0),
        (0, 68),
        (5, 88),
        (0, 52),
        (1, 179),
        (1, 300),
        (16, 56),
        (0, 300),
        (0, 43),
        (0, 0),
        (0, 92),
        (4, 287),
        (0, 0),
        (0, 7),
        (14, 155),
        (0, 4),
        (0, 24),
        (0, 2),
        (0, 13),
        (9, 189),
        (0, 0),
        (0, 6),
        (0, 167),
        (0, 185),
        (0, 39),
        (0, 52),
        (1, 10),
        (3, 63),
        (1, 46),
        (0, 24),
        (17, 84),
        (3, 295),
        (5, 266),
        (0, 25),
        (0, 7),
        (0, 36),
        (0, 70),
        (0, 294),
        (0, 65),
        (31, 38),
        (32, 42),
        (0, 3),
        (3, 75),
        (0, 112),
        (0, 122),
        (11, 186),
    ]),
    entries: ::phf::Slice::Static(&[
        ("regex_flags", Tsx::RegexFlags),
        ("type", Tsx::Type),
        ("==", Tsx::EQEQ),
        ("property_identifier", Tsx::PropertyIdentifier),
        ("&=", Tsx::AMPEQ),
        ("identifier", Tsx::Identifier),
        (">>", Tsx::GTGT),
        ("type_query", Tsx::TypeQuery),
        ("++", Tsx::PLUSPLUS),
        ("accessibility_modifier", Tsx::AccessibilityModifier),
        ("await_expression", Tsx::AwaitExpression),
        ("===", Tsx::EQEQEQ),
        ("flow_maybe_type", Tsx::FlowMaybeType),
        ("*", Tsx::STAR),
        ("formal_parameters", Tsx::FormalParameters),
        ("hash_bang_line", Tsx::HashBangLine),
        ("arguments", Tsx::Arguments),
        ("function_declaration", Tsx::FunctionDeclaration),
        ("index_type_query", Tsx::IndexTypeQuery),
        ("super", Tsx::Super),
        ("?", Tsx::QMARK),
        ("import", Tsx::Import),
        ("literal_type", Tsx::LiteralType),
        ("static", Tsx::Static),
        ("false", Tsx::False),
        ("type_annotation", Tsx::TypeAnnotation),
        ("debugger_statement", Tsx::DebuggerStatement),
        (">=", Tsx::GTEQ),
        ("arrow_function", Tsx::ArrowFunction),
        ("instanceof", Tsx::Instanceof),
        ("regex_pattern", Tsx::RegexPattern),
        ("throw", Tsx::Throw),
        ("nested_type_identifier", Tsx::NestedTypeIdentifier),
        ("template_substitution", Tsx::TemplateSubstitution),
        ("switch_case", Tsx::SwitchCase),
        ("protected", Tsx::Protected),
        ("public", Tsx::Public),
        ("switch_body", Tsx::SwitchBody),
        ("import_specifier", Tsx::ImportSpecifier),
        ("var", Tsx::Var),
        ("assignment_expression", Tsx::AssignmentExpression),
        ("abstract", Tsx::Abstract),
        ("<<=", Tsx::LTLTEQ),
        ("meta_property", Tsx::MetaProperty),
        ("ERROR", Tsx::Error),
        ("import_clause", Tsx::ImportClause),
        ("set", Tsx::Set),
        ("class_body", Tsx::ClassBody),
        ("subscript_expression", Tsx::SubscriptExpression),
        ("@", Tsx::AT),
        ("switch_body_repeat1", Tsx::SwitchBodyRepeat1),
        ("string_token2", Tsx::StringToken2),
        ("ternary_expression", Tsx::TernaryExpression),
        ("statement_block", Tsx::StatementBlock),
        ("string_repeat1", Tsx::StringRepeat1),
        ("mapped_type_clause", Tsx::MappedTypeClause),
        ("jsx_attribute", Tsx::JsxAttribute),
        ("namespace_import", Tsx::NamespaceImport),
        ("function", Tsx::Function),
        ("class_declaration", Tsx::ClassDeclaration),
        ("*=", Tsx::STAREQ),
        ("try_statement", Tsx::TryStatement),
        ("jsx_namespace_name", Tsx::JsxNamespaceName),
        (">>=", Tsx::GTGTEQ),
        ("array_pattern", Tsx::ArrayPattern),
        ("for", Tsx::For),
        ("jsx_self_closing_element", Tsx::JsxSelfClosingElement),
        ("statement_identifier", Tsx::StatementIdentifier),
        ("undefined", Tsx::Undefined),
        ("property_signature", Tsx::PropertySignature),
        ("finally_clause", Tsx::FinallyClause),
        ("variable_declaration", Tsx::VariableDeclaration),
        ("pair", Tsx::Pair),
        ("continue_statement", Tsx::ContinueStatement),
        ("readonly", Tsx::Readonly),
        ("assignment_pattern", Tsx::AssignmentPattern),
        ("_import_export_specifier", Tsx::ImportExportSpecifier),
        ("construct_signature", Tsx::ConstructSignature),
        ("!=", Tsx::BANGEQ),
        ("optional_parameter", Tsx::OptionalParameter),
        ("do_statement", Tsx::DoStatement),
        ("jsx_closing_element", Tsx::JsxClosingElement),
        ("&&", Tsx::AMPAMP),
        ("in", Tsx::In),
        ("jsx_element_repeat1", Tsx::JsxElementRepeat1),
        (
            "augmented_assignment_expression",
            Tsx::AugmentedAssignmentExpression,
        ),
        ("intersection_type", Tsx::IntersectionType),
        ("interface", Tsx::Interface),
        ("object_repeat1", Tsx::ObjectRepeat1),
        ("update_expression", Tsx::UpdateExpression),
        ("export_clause", Tsx::ExportClause),
        ("export_clause_repeat1", Tsx::ExportClauseRepeat1),
        ("require", Tsx::Require),
        ("typeof", Tsx::Typeof),
        ("_property_name", Tsx::PropertyName),
        ("type_parameter", Tsx::TypeParameter),
        ("private", Tsx::Private),
        ("**", Tsx::STARSTAR),
        ("target", Tsx::Target),
        ("|", Tsx::PIPE),
        ("abstract_method_signature", Tsx::AbstractMethodSignature),
        ("enum_declaration", Tsx::EnumDeclaration),
        ("import_alias", Tsx::ImportAlias),
        ("void", Tsx::Void),
        ("do", Tsx::Do),
        ("jsx_opening_element", Tsx::JsxOpeningElement),
        ("class_body_repeat1", Tsx::ClassBodyRepeat1),
        ("implements_clause_repeat1", Tsx::ImplementsClauseRepeat1),
        ("symbol", Tsx::Symbol),
        ("let", Tsx::Let),
        ("labeled_statement", Tsx::LabeledStatement),
        ("delete", Tsx::Delete),
        ("namespace", Tsx::Namespace),
        ("const", Tsx::Const),
        ("throw_statement", Tsx::ThrowStatement),
        ("required_parameter", Tsx::RequiredParameter),
        ("named_imports", Tsx::NamedImports),
        ("if_statement", Tsx::IfStatement),
        ("keyof", Tsx::Keyof),
        (")", Tsx::RPAREN),
        ("computed_property_name", Tsx::ComputedPropertyName),
        ("finally", Tsx::Finally),
        ("return", Tsx::Return),
        ("internal_module", Tsx::InternalModule),
        ("]", Tsx::RBRACK),
        ("parenthesized_type", Tsx::ParenthesizedType),
        ("||", Tsx::PIPEPIPE),
        ("-", Tsx::DASH),
        ("yield_expression", Tsx::YieldExpression),
        ("implements_clause", Tsx::ImplementsClause),
        ("sequence_expression", Tsx::SequenceExpression),
        ("method_definition", Tsx::MethodDefinition),
        ("string", Tsx::String),
        ("_for_header", Tsx::ForHeader),
        ("_from_clause", Tsx::FromClause),
        ("as", Tsx::As),
        ("<", Tsx::LT),
        ("case", Tsx::Case),
        ("!", Tsx::BANG),
        ("jsx_fragment", Tsx::JsxFragment),
        ("parenthesized_expression", Tsx::ParenthesizedExpression),
        ("union_type", Tsx::UnionType),
        ("module", Tsx::Module),
        ("jsx_text", Tsx::JsxText),
        (">", Tsx::GT),
        ("of", Tsx::Of),
        ("export_statement", Tsx::ExportStatement),
        ("_template_chars", Tsx::TemplateChars),
        ("array_type", Tsx::ArrayType),
        ("string_repeat2", Tsx::StringRepeat2),
        (
            "variable_declaration_repeat1",
            Tsx::VariableDeclarationRepeat1,
        ),
        ("export_statement_repeat1", Tsx::ExportStatementRepeat1),
        ("constructor_type", Tsx::ConstructorType),
        ("string_token1", Tsx::StringToken1),
        ("catch_clause", Tsx::CatchClause),
        ("switch_default", Tsx::SwitchDefault),
        ("array", Tsx::Array),
        ("public_field_definition", Tsx::PublicFieldDefinition),
        ("if", Tsx::If),
        ("class_heritage", Tsx::ClassHeritage),
        ("global", Tsx::Global),
        (
            "shorthand_property_identifier",
            Tsx::ShorthandPropertyIdentifier,
        ),
        ("^", Tsx::CARET),
        ("debugger", Tsx::Debugger),
        ("+=", Tsx::PLUSEQ),
        (
            "generator_function_declaration",
            Tsx::GeneratorFunctionDeclaration,
        ),
        ("-=", Tsx::DASHEQ),
        ("--", Tsx::DASHDASH),
        ("this", Tsx::This),
        ("return_statement", Tsx::ReturnStatement),
        ("_primary_type", Tsx::PrimaryType),
        ("member_expression", Tsx::MemberExpression),
        (",", Tsx::COMMA),
        ("true", Tsx::True),
        ("=>", Tsx::EQGT),
        ("with_statement", Tsx::WithStatement),
        ("type_predicate", Tsx::TypePredicate),
        ("is", Tsx::Is),
        ("enum_body", Tsx::EnumBody),
        ("%", Tsx::PERCENT),
        ("enum", Tsx::Enum),
        ("template_string_repeat1", Tsx::TemplateStringRepeat1),
        (">>>=", Tsx::GTGTGTEQ),
        ("type_alias_declaration", Tsx::TypeAliasDeclaration),
        ("unary_expression", Tsx::UnaryExpression),
        ("any", Tsx::Any),
        ("empty_statement", Tsx::EmptyStatement),
        ("object_pattern", Tsx::ObjectPattern),
        (".", Tsx::DOT),
        ("index_signature", Tsx::IndexSignature),
        ("_declaration", Tsx::Declaration),
        ("jsx_element", Tsx::JsxElement),
        ("(", Tsx::LPAREN),
        ("_call_signature", Tsx::CallSignature),
        ("boolean", Tsx::Boolean),
        ("escape_sequence", Tsx::EscapeSequence),
        ("function_type", Tsx::FunctionType),
        ("|=", Tsx::PIPEEQ),
        ("generic_type", Tsx::GenericType),
        ("_initializer", Tsx::Initializer),
        ("continue", Tsx::Continue),
        ("jsx_opening_element_repeat1", Tsx::JsxOpeningElementRepeat1),
        ("try", Tsx::Try),
        ("}", Tsx::RBRACE),
        ("&", Tsx::AMP),
        ("non_null_expression", Tsx::NonNullExpression),
        ("_parameter_name", Tsx::ParameterName),
        ("from", Tsx::From),
        ("null", Tsx::Null),
        ("break_statement", Tsx::BreakStatement),
        ("enum_assignment", Tsx::EnumAssignment),
        ("named_imports_repeat1", Tsx::NamedImportsRepeat1),
        ("rest_parameter", Tsx::RestParameter),
        ("as_expression", Tsx::AsExpression),
        ("\\\"", Tsx::DQUOTE),
        ("comment", Tsx::Comment),
        ("import_require_clause", Tsx::ImportRequireClause),
        ("await", Tsx::Await),
        ("...", Tsx::DOTDOTDOT),
        ("/", Tsx::SLASH),
        ("type_parameters", Tsx::TypeParameters),
        ("break", Tsx::Break),
        ("switch", Tsx::Switch),
        ("yield", Tsx::Yield),
        ("program", Tsx::Program),
        (":", Tsx::COLON),
        ("method_signature", Tsx::MethodSignature),
        ("regex", Tsx::Regex),
        ("new_expression", Tsx::NewExpression),
        ("_expression", Tsx::Expression),
        ("for_statement", Tsx::ForStatement),
        ("call_expression", Tsx::CallExpression),
        ("_automatic_semicolon", Tsx::AutomaticSemicolon),
        ("lexical_declaration", Tsx::LexicalDeclaration),
        ("+", Tsx::PLUS),
        ("constraint", Tsx::Constraint),
        ("{|", Tsx::LBRACEPIPE),
        ("end", Tsx::End),
        ("tuple_type", Tsx::TupleType),
        ("extends_clause", Tsx::ExtendsClause),
        ("generator_function", Tsx::GeneratorFunction),
        ("<<", Tsx::LTLT),
        ("/=", Tsx::SLASHEQ),
        ("new", Tsx::New),
        ("jsx_expression", Tsx::JsxExpression),
        ("while_statement", Tsx::WhileStatement),
        ("type_identifier", Tsx::TypeIdentifier),
        ("ambient_declaration", Tsx::AmbientDeclaration),
        ("lookup_type", Tsx::LookupType),
        ("object_type_repeat1", Tsx::ObjectTypeRepeat1),
        ("=", Tsx::EQ),
        ("variable_declarator", Tsx::VariableDeclarator),
        ("\'", Tsx::SQUOTE),
        ("interface_declaration", Tsx::InterfaceDeclaration),
        ("nested_identifier", Tsx::NestedIdentifier),
        ("object_type", Tsx::ObjectType),
        ("switch_statement", Tsx::SwitchStatement),
        ("object", Tsx::Object),
        (";", Tsx::SEMI),
        ("default", Tsx::Default),
        ("expression_statement", Tsx::ExpressionStatement),
        ("catch", Tsx::Catch),
        ("type_arguments", Tsx::TypeArguments),
        ("{", Tsx::LBRACE),
        (">>>", Tsx::GTGTGT),
        ("with", Tsx::With),
        ("spread_element", Tsx::SpreadElement),
        ("number", Tsx::Number),
        ("export", Tsx::Export),
        ("!==", Tsx::BANGEQEQ),
        ("`", Tsx::BQUOTE),
        ("type_parameters_repeat1", Tsx::TypeParametersRepeat1),
        ("else", Tsx::Else),
        ("function_signature", Tsx::FunctionSignature),
        ("extends", Tsx::Extends),
        ("formal_parameters_repeat1", Tsx::FormalParametersRepeat1),
        ("while", Tsx::While),
        ("enum_body_repeat1", Tsx::EnumBodyRepeat1),
        ("predefined_type", Tsx::PredefinedType),
        ("implements", Tsx::Implements),
        ("export_specifier", Tsx::ExportSpecifier),
        ("**=", Tsx::STARSTAREQ),
        ("import_statement", Tsx::ImportStatement),
        ("binary_expression", Tsx::BinaryExpression),
        ("program_repeat1", Tsx::ProgramRepeat1),
        ("template_string", Tsx::TemplateString),
        ("[", Tsx::LBRACK),
        ("get", Tsx::Get),
        ("extends_clause_repeat1", Tsx::ExtendsClauseRepeat1),
        ("~", Tsx::TILDE),
        ("array_repeat1", Tsx::ArrayRepeat1),
        ("decorator", Tsx::Decorator),
        ("existential_type", Tsx::ExistentialType),
        ("for_in_statement", Tsx::ForInStatement),
        ("^=", Tsx::CARETEQ),
        ("async", Tsx::Async),
        ("<=", Tsx::LTEQ),
        ("|}", Tsx::PIPERBRACE),
        ("${", Tsx::DOLLARLBRACE),
        ("class", Tsx::Class),
        ("default_type", Tsx::DefaultType),
        ("declare", Tsx::Declare),
        ("abstract_class_declaration", Tsx::AbstractClassDeclaration),
        ("%=", Tsx::PERCENTEQ),
    ]),
};

impl From<&str> for Tsx {
    #[inline(always)]
    fn from(key: &str) -> Self {
        KEYS.get(key).unwrap().clone()
    }
}

impl From<u16> for Tsx {
    #[inline(always)]
    fn from(x: u16) -> Self {
        unsafe { std::mem::transmute(x) }
    }
}

// Tsx == u16
impl PartialEq<u16> for Tsx {
    #[inline(always)]
    fn eq(&self, x: &u16) -> bool {
        *self == Tsx::from(*x)
    }
}

// u16 == Tsx
impl PartialEq<Tsx> for u16 {
    #[inline(always)]
    fn eq(&self, x: &Tsx) -> bool {
        *x == *self
    }
}
