module Parser.Ast


type Node =
    | StringLiteral of string
    | IntLiteral of int
    | FloatLiteral of float

    | NamespacedName of string list * Node
    | Identifier of string
    | Operator of string * bool
    | Application of Node * Node

    | Conditional of Node * Node * Node

    | ListLiteral of Node list

    | SkipPattern
    | NameBind of Node
    | LiteralPattern of Node
    | ConstructorPattern of Node * Node list
    | ListPattern of Node list
    | HeadTailPattern of Node * Node

    | Case of Node * Node option * Node
    | Match of Node * Node list

    | Function of Node * Node

    | MonadBind of Node * Node

    | Compound of Node list

    | Equation of Node * Node

    | ConstructorDeclaration of Node * Node list
    | TypeDeclaration of Node * Node list * Node list

    | ImportNamespace of string * Node option
    | ImportAll of string * Node option
    | ImportFrom of string * Node list * Node option
    | Entrypoint of Node

    | Eval of Node

    | Program of Node list
