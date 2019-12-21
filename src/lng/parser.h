#pragma once
#include "lexer.h"
#include <unordered_map>
#include <stack>
#include <functional>
enum ast_type : u8 {
    NodeType_Null,
    NodeType_Global,
    NodeType_VarDecl,
    NodeType_TypeDecl,
    NodeType_FunctionDecl,
    NodeType_FunctionCall,
    NodeType_Literal,
    NodeType_Assignment,
    NodeType_TypeAlias,
    NodeType_Identifier,
    NodeType_MemberAccess,
    NodeType_MemberVarDecl,
    NodeType_MemberFuncDecl,
    NodeType_Sizeof,
    NodeType_Paren,
    NodeType_BinaryOp,
    NodeType_Return,
    NodeType_NSDecl,
    NodeType_If,
    NodeType_Else,
    NodeType_While,
    NodeType_Address,
    NodeType_Deref,
    NodeType_Index,
    NodeType_Type,
    NodeType_Negate,
    NodeType_Not,
    NodeType_As,
    NodeType_Break,
    NodeType_Continue,
    NodeType_BraceInit,
};

struct compiler;

char* ToString(ast_type Type) {
    switch (Type) {
        case NodeType_Null:            return "null";
        case NodeType_Global:          return "global";
        case NodeType_VarDecl:         return "var decl";
        case NodeType_FunctionDecl:    return "function decl";
        case NodeType_FunctionCall:    return "function call";
        case NodeType_Literal:         return "literal";
        case NodeType_Assignment:      return "assignment";
        case NodeType_TypeDecl:        return "type decl";
        case NodeType_TypeAlias:       return "type alias";
        case NodeType_Identifier:       return "var access";
        case NodeType_MemberAccess:    return "member access";
        case NodeType_MemberVarDecl:      return "member decl";
        case NodeType_Sizeof:          return "sizeof";
        case NodeType_Paren:           return "paren";
        case NodeType_BinaryOp:        return "binary op";
        case NodeType_Return:          return "return";
        case NodeType_NSDecl:          return "namespace decl";
        case NodeType_If:              return "if";
        case NodeType_While:           return "while";
        case NodeType_Address:         return "address";
        case NodeType_Deref:           return "deref";
        default:                       return "unknown";
    }
}

enum class binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    Eq,
    NEq,
    GT,
    LT,
    GET,
    LET,
    LAnd,
    LOr,
    Count,
};
std::string ToString(binop Op) {
#define CASE(x) case binop::x: return #x
    switch (Op) {
        CASE(Add);
        CASE(Sub);
        CASE(Mul);
        CASE(Div);
        CASE(Mod);
        CASE(AddEq);
        CASE(SubEq);
        CASE(MulEq);
        CASE(DivEq);
        CASE(ModEq);
        CASE(Eq);
        CASE(NEq);
        CASE(GT);
        CASE(LT);
        CASE(GET);
        CASE(LET);
        CASE(LAnd);
        CASE(LOr);
        default: return "Unknown";
    }
#undef CASE
}
enum class binop_type {
    Arithmetic,
    ArithmeticAssign,
    Equality,
    Comparison,
    Logic
};

binop_type GetBinopType(binop Op) {
    switch (Op) {
        case binop::Add:
        case binop::Sub:
        case binop::Mul:
        case binop::Div:
        case binop::Mod: return binop_type::Arithmetic;
        case binop::AddEq:
        case binop::SubEq:
        case binop::MulEq:
        case binop::DivEq:
        case binop::ModEq: return binop_type::ArithmeticAssign;
        case binop::Eq:
        case binop::NEq: return binop_type::Equality;
        case binop::GT:
        case binop::LT:
        case binop::GET:
        case binop::LET: return binop_type::Comparison;
        case binop::LAnd:
        case binop::LOr: return binop_type::Logic;
    }
    assert(0);
    return {};
}
bool IsAssignment(binop Op) {
    return GetBinopType(Op) == binop_type::ArithmeticAssign;
}

template<class enum_t>
constexpr auto ToInt(enum_t Val) {
    return (std::underlying_type_t<enum_t>)Val;
}

struct node;
struct node_global;
struct node_typedecl;
struct node_vardecl;
struct node_functiondecl;
struct node_functioncall;
struct node_literal;
struct node_assignment;
struct scope;

enum ast_flag {
    AstFlag_Known = 0x1,
    AstFlag_Linked = 0x2,
    AstFlag_IsExpression = 0x4,
};

struct node {
    ast_type AstType = NodeType_Null;
    location Location = {};
    node* Parent = 0;
    u32 ID = (u32)-1;
    u64 CustomData = 0;
    u32 Flags = 0;
    inline node(ast_type Type) : AstType(Type) {
        assert(Type != NodeType_Null);
    }
};

struct node_global : node {
    std::vector<node*> Declarations;
    std::vector<node_functiondecl*> BinaryOperators[ToInt(binop::Count)];
    std::vector<node_functiondecl*> AsOperators;
    std::vector<node_functiondecl*> NegateOperators;
    inline node_global() : node(NodeType_Global) {}
};

enum type {
    Null,
    Identifier,
    Pointer,
    Array
};

struct node_type : node {
    type Type = type::Null;
    inline node_type(type Type) : Type(Type), node(NodeType_Type) {
        assert(Type != type::Null);
    }
};

#define TYPE_AUTO ((node_type*)-1)

struct node_type_identifier : node_type {
    string_view Identifier;
    node_typedecl* Decl = 0;
    inline node_type_identifier() : node_type(type::Identifier) {}
};

struct node_type_pointer : node_type {
    node_type* Next = 0;
    inline node_type_pointer() : node_type(type::Pointer) {}
};

struct node_type_array : node_type {
    node_type* Next = 0;
    u32 Count = 0;
    inline node_type_array() : node_type(type::Array) {}
};

struct node_expression : node {
    node_type* Type = 0;
    inline node_expression(ast_type AstType) : node(AstType) {
        Flags |= AstFlag_IsExpression;
    }
};

struct node_functiondecl : node {
    string_view Identifier = {};
    std::vector<node_vardecl*> Arguments;
    scope* Scope = 0;
    std::vector<node*> Statements = {};
    node_type* ReturnType = 0;
    bool IsInternal = false;
    inline node_functiondecl() : node(NodeType_FunctionDecl) {}
};

struct node_functioncall : node_expression {
    string_view Identifier = {};
    node_functiondecl* Declaration = 0;
    std::vector<node_expression*> Arguments;
    inline node_functioncall() : node_expression(NodeType_FunctionCall) {}
};

struct node_vardecl : node {
    string_view Identifier = {};
    node_expression* InitialExpression = 0;
    node_type* Type = 0;
    inline node_vardecl() : node(NodeType_VarDecl) {}
};

struct node_membervardecl : node {
    node_vardecl* Declaration = 0;
    struct {
        node_expression* Expression = 0;
        u64 Value = 0;
        bool Custom = 0;
    } Offset;
    bool IsInternal = false;
    inline node_membervardecl() : node(NodeType_MemberVarDecl) {}
};

struct node_memberfuncdecl : node {
    node_functiondecl* Declaration = 0;
    inline node_memberfuncdecl () : node(NodeType_MemberFuncDecl) {}
};

struct node_typedecl : node {
    string_view Identifier = {};
    std::vector<node*> Members = {};
    struct {
        node_expression* Expression = 0;
        u64 Value = 0;
        bool Custom = false;
    } Align, Packing;
    u64 Size = 0;
    inline node_typedecl() : node(NodeType_TypeDecl) {}
};

struct node_literal : node_expression {
    union value {
        string_view Str;
        u64 Int;
        value() { memset(this, 0, sizeof(*this)); }
    } Value;
    data_type LiteralType = {};
    inline node_literal() : node_expression(NodeType_Literal) {
        Flags |= AstFlag_Known;
    }
};

struct node_assignment : node {
    node_expression* Assigned = 0; // "node_identifier" or "node_memberaccess"
    node_expression* Expression = 0;
    inline node_assignment() : node(NodeType_Assignment) {}
};

struct node_typealias : node {
    string_view NewIdentifier, OldIdentifier;
    inline node_typealias() : node(NodeType_TypeAlias) {}
};

// NOTE: 'Type' is valid if 'Declaration' is 'node_vardecl'
struct node_identifier : node_expression {
    string_view Identifier;
    node* Declaration = 0;
    inline node_identifier() : node_expression(NodeType_Identifier) {}
};

struct node_memberaccess : node_expression {
    node_expression* First = 0;
    node_expression* Second = 0;
    inline node_memberaccess() : node_expression(NodeType_MemberAccess) {}
};

struct node_sizeof : node_expression {
    node_type* TestType = 0;
    inline node_sizeof() : node_expression(NodeType_Sizeof) {}
};

struct node_paren : node_expression {
    node_expression* Expression = 0;
    inline node_paren() : node_expression(NodeType_Paren) {}
};

struct node_binaryop : node_expression {
    node_expression* FirstExpression = 0;
    node_expression* SecondExpression = 0;
    binop Op = binop::Count;
    node_functiondecl* Declaration = 0;
    inline node_binaryop() : node_expression(NodeType_BinaryOp) {}
};

struct node_return : node {
    node_expression* Expression = 0;
    inline node_return() : node(NodeType_Return) {}
};

struct node_nsdecl : node {
    string_view Identifier;
    std::vector<node*> Declarations;
    inline node_nsdecl() : node(NodeType_NSDecl) {}
};

struct node_if : node {
    node* Condition = 0;
    std::vector<node*> Statements;
    inline node_if() : node(NodeType_If) {}
};

struct node_else : node {
    std::vector<node*> Statements;
    inline node_else() : node(NodeType_Else) {}
};

struct node_while : node {
    node* Condition = 0;
    std::vector<node*> Statements;
    inline node_while() : node(NodeType_While) {}
};

struct node_address : node_expression {
    node_expression* Expression = 0;
    inline node_address() : node_expression(NodeType_Address) {}
};

struct node_deref : node_expression {
    node_expression* Expression = 0;
    inline node_deref() : node_expression(NodeType_Deref) {}
};

struct node_negate : node_expression {
    node_expression* Expression = 0;
    node_functiondecl* Declaration = 0;
    inline node_negate() : node_expression(NodeType_Negate) {}
};

struct node_not : node_expression {
    node_expression* Expression = 0;
    inline node_not() : node_expression(NodeType_Not) {}
};

struct node_as : node_expression {
    node_expression* Expression = 0;
    node_functiondecl* Decl = 0;
    inline node_as() : node_expression(NodeType_As) {}
};

struct node_index : node_expression {
    string_view Identifier;
    node_expression* Expression = 0;
    node_expression* IndexExpression = 0;
    inline node_index() : node_expression(NodeType_Index) {}
};

struct node_break : node {
    node* Statement = 0; // while
    inline node_break() : node(NodeType_Break) {}
};

struct node_continue : node {
    node* Statement = 0; // while
    inline node_continue() : node(NodeType_Continue) {}
};

struct node_braceinit : node_expression {
    inline node_braceinit() : node_expression(NodeType_BraceInit) {}
};

#define TOKEN_ADD "+"
#define TOKEN_SUB "-"
#define TOKEN_MUL "*"
#define TOKEN_DIV "/"
#define TOKEN_MOD "%"
#define TOKEN_NOT "!"
#define TOKEN_ADDRESS "@"
#define TOKEN_DEREF "^"
#define TOKEN_POINTER "^"
#define TOKEN_MEMBER "."
#define TOKEN_PAREN_BEGIN "("
#define TOKEN_PAREN_END ")"
#define TOKEN_SUBSRCIPT_BEGIN "["
#define TOKEN_SUBSRCIPT_END "]"
#define TOKEN_BODY_BEGIN "{"
#define TOKEN_BODY_END "}"
#define TOKEN_BRACEINIT_BEGIN "{"
#define TOKEN_BRACEINIT_END "}"
#define TOKEN_ARGLIST_BEGIN "("
#define TOKEN_ARGLIST_END ")"
#define TOKEN_RETTYPE_DELIM ":"

u32 Append(message& Message, node_type* Type) {
    auto Column = Message.Column;
    Message.Column = {};
    u32 Result = [&]() {
        switch (Type->Type) {
            case type::Identifier: {
                auto Ident = (node_type_identifier*)Type;
                return Append(Message, Ident->Identifier);
            }
            case type::Pointer: {
                auto Ptr = (node_type_pointer*)Type;
                return Append(Message, TOKEN_POINTER) + Append(Message, Ptr->Next);
            }
            case type::Array: {
                auto Arr = (node_type_array*)Type;
                return Append(Message, TOKEN_SUBSRCIPT_BEGIN) + Append(Message, Arr->Count) + Append(Message, TOKEN_SUBSRCIPT_END) + Append(Message, Arr->Next);
            }
        }
        assert(0);
    }();
    if (Result < Column.Size) {
        Append(Message, std::string(Column.Size - Result, ' '));
        return Column.Size;
    }
    return Result;
}

bool Equals(node_type* A, node_type* B) {
    if (A->Type == B->Type) {
        switch (A->Type) {
            case type::Identifier: {
                auto a = (node_type_identifier*)A;
                auto b = (node_type_identifier*)B;
                return a->Decl == b->Decl;
            }
            case type::Pointer: {
                auto a = (node_type_pointer*)A;
                auto b = (node_type_pointer*)B;
                return Equals(a->Next, b->Next);
            }
            case type::Array: {
                auto a = (node_type_array*)A;
                auto b = (node_type_array*)B;
                return a->Count == b->Count && Equals(a->Next, b->Next);
            }
        }
    }
    else {
        return false;
    }
    assert(0);
    return 0;
}

std::string ToString(node_type* Type) {
    message Msg;
    Append(Msg, Type);
    return ToString(Msg.String);
}

struct scope {
    scope* Parent = 0;
    std::vector<scope*> Children;
    std::unordered_map<string_view, node_typedecl*> Types;
};

string_view GetIdentifier(node_type* Type) {
    switch (Type->Type) {
        case type::Identifier: return ((node_type_identifier*)Type)->Identifier;
        case type::Pointer: return GetIdentifier(((node_type_pointer*)Type)->Next);
        case type::Array: return GetIdentifier(((node_type_array*)Type)->Next);
    }
    assert(0);
    return {};
}
string_view GetIdentifier(node* Node) {
    switch (Node->AstType) {
        case NodeType_VarDecl: return ((node_vardecl*)Node)->Identifier;
        case NodeType_NSDecl: return ((node_nsdecl*)Node)->Identifier;
        case NodeType_MemberVarDecl: return ((node_membervardecl*)Node)->Declaration->Identifier;
        case NodeType_MemberFuncDecl: return ((node_memberfuncdecl*)Node)->Declaration->Identifier;
        case NodeType_Identifier: return ((node_identifier*)Node)->Identifier;
        case NodeType_TypeDecl: return ((node_typedecl*)Node)->Identifier;
        case NodeType_FunctionCall: return ((node_functioncall*)Node)->Identifier;
        case NodeType_FunctionDecl: return ((node_functiondecl*)Node)->Identifier;
    }
    assert(0);
    return {};
}
bool IsStatement(node* Node) {
    switch (Node->AstType) {
        case NodeType_MemberAccess:
            return IsStatement(((node_memberaccess*)Node)->Second);
        case NodeType_BinaryOp:
            return IsAssignment(((node_binaryop*)Node)->Op);
        case NodeType_Assignment:
        case NodeType_Return:
        case NodeType_FunctionDecl:
        case NodeType_TypeDecl:
        case NodeType_TypeAlias:
        case NodeType_VarDecl:
        case NodeType_FunctionCall:
            return true;
    }
    return false;
}

#define PARSE_OK            0 
#define PARSE_OTHER         1 // Tokens may mean something else
#define PARSE_FINISH        2 // Reached end of file
#define PARSE_FAIL          3 
#define PARSE_FAIL_CONTINUE 4 

#define LINK_OK 0
#define LINK_ERROR 1
#define LINK_FATAL 2

template<class val_t, class step_t>
val_t Ceil(val_t Val, step_t Step) {
    return (val_t)(ceilf((f32)Val / Step) * Step);
}
enum class build_type {
    Null, App, SLib, DLib
};
enum class arch_type {
    Null,
    x86,
    x64
};
struct arch {
    arch_type Type = arch_type::Null;
    u32 PtrSize = 0;
};

arch CreateArch(string_view Name, location Location = {}) {
    arch Result;
    if (Name == "x86") {
        Result.Type = arch_type::x86;
        Result.PtrSize = 4;
    }
    else if (Name == "x64") {
        Result.Type = arch_type::x64;
        Result.PtrSize = 8;
    }
    else {
        ReportError(Location, "Available arch are: 'x86', 'x64'; got '", Name, "'");
    }
    return Result;
}

bool CanTakeAddressOf(node* Node) {
    switch (Node->AstType) {
        case NodeType_Identifier:
            return true;
        case NodeType_Index:
            // TODO 
            return true;
        default:
            return false;
    }
}

bool CanDeclareFunction(node* Node) {
    return
        Node->AstType == NodeType_Global ||
        Node->AstType == NodeType_NSDecl ||
        Node->AstType == NodeType_TypeDecl ||
        Node->AstType == NodeType_FunctionDecl;
}
bool CanDeclareVariable(node* Node) {
    return
        Node->AstType == NodeType_Global ||
        Node->AstType == NodeType_If ||
        Node->AstType == NodeType_Else ||
        Node->AstType == NodeType_While ||
        Node->AstType == NodeType_NSDecl ||
        Node->AstType == NodeType_FunctionDecl ||
        Node->AstType == NodeType_TypeDecl;
}
bool CanDeclareNamespace(node* Node) {
    return
        Node->AstType == NodeType_Global ||
        Node->AstType == NodeType_NSDecl;
}
bool CanContainBreak(node* Node) {
    return
        Node->AstType == NodeType_While;
}
bool CanContainContinue(node* Node) {
    return
        Node->AstType == NodeType_While;
}


node* GetNextHolder(node* Node, bool (*Pred)(node*)) {
    Node = Node->Parent;
    while (true) {
        if (Node)
            if (Pred(Node))
                return Node;
            else
                Node = Node->Parent;
        else
            return 0;
    }
}

node* GetNextFunctionHolder(node* Node) { return GetNextHolder(Node, CanDeclareFunction); }
node* GetNextVariableHolder(node* Node) { return GetNextHolder(Node, CanDeclareVariable); }
node* GetNextNamespaceHolder(node* Node) { return GetNextHolder(Node, CanDeclareNamespace); }
node* GetNextBreakHolder(node* Node) { return GetNextHolder(Node, CanContainBreak); }
node* GetNextContinueHolder(node* Node) { return GetNextHolder(Node, CanContainContinue); }

#define TOKEN Lexer->CursorToken

bool HasDefaultArguments(node_functiondecl* Decl) {
    for (auto Arg : Decl->Arguments)
        if (Arg->InitialExpression)
            return true;
    return false;
}

struct parser {
    // Input
    lexer* Lexer = 0;

    // Output
    build_type BuildType = build_type::App;
    string_view OutFile = "a.exe";
    std::string BinDir = "<filedir>bin\\";
    std::string TempDir = "<filedir>temp\\";
    string_view TranslatorName = "cpp";
    arch TargetArch = CreateArch("x64");

    // State
    string_view BinaryOperators[ToInt(binop::Count)];
    node_global* GlobalNode = {};
    b32 Success = {};
    scope* GlobalScope = {};
    scope* CurrentScope = {};
    u32 NodeIDCounter = 0;
    string_view CustomBinDir;
    string_view CustomTempDir;
    std::vector<std::string> ImportDirectories;
    std::vector<void*> DeleteAfterLink;

    parser(lexer* Lexer) : Lexer(Lexer) {}

#pragma push_macro("new")
#undef new
    struct memory {
        struct block {
            static constexpr size_t Capacity = 1024 * 1024;
            char Data[Capacity];
            block* Next = 0;
        };
        block* FirstBlock = 0;
        block* LastBlock = 0;
        char* Cursor = 0;
        u32 BlocksUsed = 0;
        memory() {
            FirstBlock = LastBlock = AllocateBlock();
            Cursor = FirstBlock->Data;
        }
        block* AllocateBlock() {
            ++BlocksUsed;
            auto Result = VirtualAlloc((void*)(BlocksUsed * sizeof(block)), sizeof(block), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
            return new(Result) block;
        }
        void AppendBlock() {
            LastBlock->Next = AllocateBlock();
            LastBlock = LastBlock->Next;
            Cursor = LastBlock->Data;
        }
        void* Allocate(size_t Size) {
#if 1
            auto GetRemainingCapacity = [&]() {
                return LastBlock->Data + block::Capacity - Cursor;
            };
            if (Size > GetRemainingCapacity()) {
                AppendBlock();
            }
            auto Result = Cursor;
            Cursor += Size;
            return Result;
#else 
            return operator new(Size);
#endif
        }
        template<class type>
        type* Allocate() {
            return new(Allocate(sizeof(type))) type;
        }
    } Memory;
#pragma pop_macro("new")

    bool NextToken() { return Lexer->NextToken(); }
    bool PrevToken() { return Lexer->PrevToken(); }
    token PeekToken() { return Lexer->PeekToken(); }
    bool Expect(string_view Str) { return Lexer->Expect(Str); }
    bool ExpectStringLiteral() { return Lexer->ExpectStringLiteral(); }
    bool ExpectIntLiteral() { return Lexer->ExpectIntLiteral(); }
    bool ExpectIdentifier() { return Lexer->ExpectIdentifier(); }

    void Free();
    void Run(compiler* Compiler);
    int ParseIteration();
    bool ParseMainLoop();
    bool ParseImportLoop();

    template<class node_t>
    node_t* CreateNode(location Location = {}) {
        return CreateNode<node_t>(0, Location);
    }
    template<class node_t>
    node_t* CreateNode(node* Parent, location Location = {}) {
        node_t* Result = Memory.Allocate<node_t>();
        Result->ID = NodeIDCounter++;
        Result->Parent = Parent;
        Result->Location = Location;
        return Result;
    }
    template<class node_t>
    node_t* CreateType(location Location = {}) {
        node_t* Result = Memory.Allocate<node_t>();
        Result->Location = Location;
        return Result;
    }
    node_type_identifier* CreateTypeIdentifier(node_typedecl* TypeDecl) {
        auto Result = CreateType<node_type_identifier>();
        Result->Decl = TypeDecl;
        Result->Identifier = TypeDecl->Identifier;
        return Result;
    }
    node_type_identifier* CreateTypeIdentifier(location Location, node_typedecl* TypeDecl) {
        auto Result = CreateType<node_type_identifier>(Location);
        Result->Decl = TypeDecl;
        Result->Identifier = TypeDecl->Identifier;
        return Result;
    }
    node_type_identifier* CreateTypeIdentifier(token IdentToken) {
        auto Result = CreateType<node_type_identifier>(IdentToken.Location);
        Result->Identifier = IdentToken.Value;
        return Result;
    }
    node_type_pointer* CreateTypePointer(location Location) {
        auto Result = CreateType<node_type_pointer>(Location);
        return Result;
    }
    node_type_pointer* CreateTypePointer() {
        auto Result = CreateType<node_type_pointer>({});
        return Result;
    }
    node_type_pointer* CreateTypePointer(node_type* PointsTo) {
        auto Result = CreateType<node_type_pointer>({});
        Result->Next = PointsTo;
        return Result;
    }
    node_type_array* CreateTypeArray(location Location) {
        auto Result = CreateType<node_type_array>(Location);
        return Result;
    }

    node_global* CreateGlobalNode() {
        node_global* Result = new node_global;
        return Result;
    }
    node_typedecl* CreateTypeDecl(location Location, string_view Identifier, u32 Size = 0) {
        node_typedecl* Result = CreateNode<node_typedecl>(Location);
        Result->Identifier = Identifier;
        Result->Size = Result->Align.Value = Size;
        return Result;
    }
    node_typedecl* CreateTypeDecl(node* Parent, location Location, string_view Identifier, u32 Size = 0) {
        node_typedecl* Result = CreateNode<node_typedecl>(Parent, Location);
        Result->Identifier = Identifier;
        Result->Size = Result->Align.Value = Size;
        return Result;
    }
    node_vardecl* CreateVarDecl(location Location, string_view Identifier, node_type* Type, node_expression* InitialExpression) {
        node_vardecl* Result = CreateNode<node_vardecl>(Location);
        Result->Type = Type;
        Result->Identifier = Identifier;
        Result->InitialExpression = InitialExpression;
        return Result;
    }
    node_vardecl* CreateVarDecl(node* Parent) {
        return CreateNode<node_vardecl>(Parent);
    }
    node_vardecl* CreateVarDecl(string_view Identifier, node_type* Type) {
        node_vardecl* Result = CreateNode<node_vardecl>({});
        Result->Type = Type;
        Result->Identifier = Identifier;
        return Result;
    }
    node_literal* CreateLiteral(location Location, string_view Value, data_type Type) {
        node_literal* Result = CreateNode<node_literal>(Location);
        Result->LiteralType = Type;
        Result->Value.Str = Value;
        return Result;
    }
    node_literal* CreateLiteral(location Location, u64 Value) {
        node_literal* Result = CreateNode<node_literal>(Location);
        Result->LiteralType = data_type::UInt;
        Result->Value.Int = Value;
        return Result;
    }
    node_literal* CreateLiteral(token LiteralToken) {
        switch (LiteralToken.DataType) {
            case data_type::UInt: return CreateLiteral(LiteralToken.Location, ToU64_Unchecked(LiteralToken.Value));
            default: return CreateLiteral(LiteralToken.Location, LiteralToken.Value, LiteralToken.DataType);
        }
    }
    node_assignment* CreateAssignment(location Location) {
        node_assignment* Result = CreateNode<node_assignment>(Location);
        return Result;
    }
    node_functioncall* CreateFuncCall(location Location, string_view Identifier) {
        node_functioncall* Result = CreateNode<node_functioncall>(Location);
        Result->Identifier = Identifier;
        return Result;
    }
    node_functiondecl* CreateFuncDecl(node* Parent) {
        node_functiondecl* Result = CreateNode<node_functiondecl>(Parent);
        return Result;
    }
    node_functiondecl* CreateFuncDecl(location Location, string_view Identifier) {
        node_functiondecl* Result = CreateNode<node_functiondecl>(Location);
        Result->Identifier = Identifier;
        return Result;
    }
    node_typealias* CreateTypeAlias(location Location, string_view OldIdentifier, string_view NewIdentifier) {
        node_typealias* Result = CreateNode<node_typealias>(Location);
        Result->NewIdentifier = NewIdentifier;
        Result->OldIdentifier = OldIdentifier;
        return Result;
    }
    node_identifier* CreateVarAccess(location Location, string_view Identifier) {
        node_identifier* Result = CreateNode<node_identifier>(Location);
        Result->Identifier = Identifier;
        return Result;
    }
    node_memberaccess* CreateMemberAccess(location Location) {
        node_memberaccess* Result = CreateNode<node_memberaccess>(Location);
        return Result;
    }
    node_membervardecl* CreateMemberVarDecl(location Location, node_vardecl* Declaration) {
        node_membervardecl* Result = CreateNode<node_membervardecl>(Location);
        Result->Declaration = Declaration;
        Declaration->Parent = Result;
        return Result;
    }
    node_membervardecl* CreateMemberVarDecl(node_typedecl* Parent, node_vardecl* Declaration, bool IsInternal = false) {
        node_membervardecl* Result = CreateNode<node_membervardecl>(Parent);
        Result->Declaration = Declaration;
        Result->IsInternal = IsInternal;
        Declaration->Parent = Result;
        return Result;
    }
    node_membervardecl* CreateMemberVarDecl(node_vardecl* Declaration) {
        node_membervardecl* Result = CreateNode<node_membervardecl>({});
        Result->Declaration = Declaration;
        Declaration->Parent = Result;
        return Result;
    }
    node_membervardecl* CreateMemberVarDecl(location Location) {
        node_membervardecl* Result = CreateNode<node_membervardecl>(Location);
        return Result;
    }
    node_membervardecl* CreateMemberVarDecl(node_typedecl* Parent) {
        node_membervardecl* Result = CreateNode<node_membervardecl>({});
        return Result;
    }
    node_memberfuncdecl* CreateMemberFuncDecl(location Location, node_functiondecl* Declaration) {
        node_memberfuncdecl* Result = CreateNode<node_memberfuncdecl>(Location);
        Result->Declaration = Declaration;
        Declaration->Parent = Result;
        return Result;
    }
    node_sizeof* CreateSizeof(location Location, node_type* Type) {
        node_sizeof* Result = CreateNode<node_sizeof>(Location);
        Result->TestType = Type;
        return Result;
    }
    node_paren* CreateParen(location Location) {
        node_paren* Result = CreateNode<node_paren>(Location);
        return Result;
    }
    node_binaryop* CreateBinaryOp(location Location, binop Op) {
        node_binaryop* Result = CreateNode<node_binaryop>(Location);
        Result->Op = Op;
        return Result;
    }
    node_return* CreateReturn(location Location) {
        node_return* Result = CreateNode<node_return>(Location);
        return Result;
    }
    node_nsdecl* CreateNSDecl(location Location, string_view Identifier) {
        node_nsdecl* Result = CreateNode<node_nsdecl>(Location);
        Result->Identifier = Identifier;
        return Result;
    }

    node_type* ParseType();
    u32 ParseCode = 0;
    node_vardecl* ParseVarDeclInitialExpression(token Identifier, node_type* Type);
    node_vardecl* ParseVarDecl();
    node_expression* ParseExpression();
    node_expression* ParseExpressionNoBinop();
    node_expression* ParseExpressionNoBinopNoAs();
    node_functioncall* ParseFunctionCall();
    node_functiondecl* ParseFunDeclArgsAndBody(location Location, string_view Identifier);
    node* ParseDeclaration();
    node* ParseStatement(const std::vector<node*>& ParentStatements);

    int LinkNodes();
    int LinkNode(node_assignment* Assignment);
    int LinkNode(node_vardecl* VarDecl);
    int LinkHeader(node_functiondecl* FunctionDecl);
    int LinkNode(node_functiondecl* FunctionDecl);
    int LinkNode(node_typedecl* TypeDecl);
    int LinkNode(node_typealias* TypeAlias);
    int LinkNode(node_identifier* VarAccess);
    int LinkNode(node_functioncall* Call);
    int LinkNode(node_memberaccess* MemberAccess);
    int LinkNode(node_sizeof* Sizeof);
    int LinkNode(node_paren* Paren);
    int LinkNode(node_literal* Literal);
    int LinkNode(node_binaryop* BinaryOp);
    int LinkNode(node_return* Return);
    int LinkNode(node_nsdecl* NSDecl);
    int LinkNode(node_if* If);
    int LinkNode(node_else* Else);
    int LinkNode(node_while* While);
    int LinkNode(node_address* Address);
    int LinkNode(node_deref* Deref);
    int LinkNode(node_index* Index);
    int LinkNode(node_negate* Index);
    int LinkNode(node_not* Index);
    int LinkNode(node_as* Index);
    int LinkNode(node_break* Index);
    int LinkNode(node_continue* Index);
    int LinkNode(node_braceinit* Braceinit);
    int LinkNode(node* Node);

    binop BinopFromString(string_view Op) {
        for (int i=0; i < ToInt(binop::Count); ++i) {
            if (BinaryOperators[i] == Op)
                return (binop)i;
        }
        assert(0);
        return {};
    }
    string_view BinopString(binop Op) {
        return BinaryOperators[ToInt(Op)];
    }
    bool IsBinaryOperator(string_view Str) {
        for (int i=0; i < ARRAYSIZE(BinaryOperators); ++i) {
            if (BinaryOperators[i] == Str)
                return true;
        }
        return false;
    }

    node_typedecl* GetTypeDecl(node_type* Type) {
        switch (Type->Type) {
            case type::Array: return GetTypeDecl(((node_type_array*)Type)->Next);
            case type::Pointer: return GetTypeDecl(((node_type_pointer*)Type)->Next);
            case type::Identifier: return ((node_type_identifier*)Type)->Decl;
        }
        assert(0);
        return 0;
    }
    node_typedecl* GetTypeDecl(node* Node) {
        switch (Node->AstType) {
            case NodeType_Literal:
                return GlobalScope->Types[ToString((((node_literal*)Node)->LiteralType))];
            case NodeType_MemberAccess:
                return GetTypeDecl(((node_memberaccess*)Node)->Second->Type);
            case NodeType_Sizeof:
                return Type_uint64;
            case NodeType_Paren:
                return GetTypeDecl(((node_paren*)Node)->Expression->Type);
            case NodeType_VarDecl:
            case NodeType_FunctionDecl:
            case NodeType_Identifier:
            case NodeType_BinaryOp:
            case NodeType_FunctionCall:
            case NodeType_MemberVarDecl:
            case NodeType_Address:
            case NodeType_Deref:
            case NodeType_Negate:
            case NodeType_Not:
                return GetTypeDecl(GetType(Node));
        }
        assert(0);
    }
    node_type* GetType(node* Node) {
        if (Node->Flags & AstFlag_IsExpression) {
            return ((node_expression*)Node)->Type;
        }
        switch (Node->AstType) {
            case NodeType_VarDecl:
                return ((node_vardecl*)Node)->Type;
            case NodeType_FunctionDecl:
                return ((node_functiondecl*)Node)->ReturnType;
            case NodeType_MemberVarDecl:
                return ((node_membervardecl*)Node)->Declaration->Type;
        }
        assert(0);
    }
    node* GetDeclaration(node* Node) {
        switch (Node->AstType) {
            case NodeType_Identifier: return ((node_identifier*)Node)->Declaration;
            case NodeType_FunctionCall: return ((node_functioncall*)Node)->Declaration;
            case NodeType_MemberAccess: return GetDeclaration(((node_memberaccess*)Node)->First);
            case NodeType_Literal: {
                auto Literal = (node_literal*)Node;
                switch (Literal->LiteralType) {
                    case data_type::String: return Type_string;
                    default:
                        assert(0);
                }
            }
            default:
                assert(0);
        }
    }
    std::vector<node*>* GetChildren(node* Node) {
        switch (Node->AstType) {
            case NodeType_Global:
                return &((node_global*)Node)->Declarations;
            case NodeType_NSDecl:
                return &((node_nsdecl*)Node)->Declarations;
            case NodeType_FunctionDecl:
                return &((node_functiondecl*)Node)->Statements;
            case NodeType_If:
                return &((node_if*)Node)->Statements;
            case NodeType_Else:
                return &((node_else*)Node)->Statements;
            case NodeType_While:
                return &((node_while*)Node)->Statements;
            case NodeType_TypeDecl:
                return (std::vector<node*>*) & ((node_typedecl*)Node)->Members;
            case NodeType_Type: {
                auto Type = (node_type_identifier*)Node;
                assert(Type->Type == type::Identifier);
                return (std::vector<node*>*) & Type->Decl->Members;
            }
            default:
                assert(false);
                return 0;
        }
    }
    std::vector<node*>* GetChildrenOfClosestScope(node* Node) {
        switch (Node->AstType) {
            case NodeType_Global:
                return &((node_global*)Node)->Declarations;
            case NodeType_FunctionDecl:
                return &((node_functiondecl*)Node)->Statements;
            case NodeType_NSDecl:
                return &((node_nsdecl*)Node)->Declarations;
            default:
                return GetChildrenOfClosestScope(Node->Parent);
        }
    }

    node_type_identifier* GetFittingIntType(s64 Val) {
        if (Val < 0) {
            if (Val < INT32_MIN) return TypeIdentifier_int64;
            if (Val < INT16_MIN) return TypeIdentifier_int32;
            if (Val < INT8_MIN) return TypeIdentifier_int16;
            return TypeIdentifier_int8;
        }
        else {
            if (Val > UINT32_MAX) return TypeIdentifier_uint64;
            if (Val > UINT16_MAX) return TypeIdentifier_uint32;
            if (Val > UINT8_MAX) return TypeIdentifier_uint16;
            return TypeIdentifier_uint8;
        }
        assert(0);
    }

    // TODO: deal with ranges, big values, signness
    s64 EvaluateInt(node* Node) {
        assert(Node->Flags & AstFlag_Known);
        switch (Node->AstType) {
            case NodeType_Literal: {
                auto Literal = (node_literal*)Node;
                assert(Literal->LiteralType == data_type::UInt);
                return Literal->Value.Int;
            }
            case NodeType_Sizeof: {
                auto Sizeof = (node_sizeof*)Node;
                return GetSizeOf(Sizeof->Type);
            }
            case NodeType_BinaryOp: {
                auto Binop = (node_binaryop*)Node;
                auto First = EvaluateInt(Binop->FirstExpression);
                auto Second = EvaluateInt(Binop->SecondExpression);
                switch (Binop->Op) {
                    case binop::Add: return First + Second;
                    case binop::Sub: return First - Second;
                    case binop::Mul: return First * Second;
                    case binop::Div: return First / Second;
                    case binop::Mod: return First % Second;
                }
                assert(0);
            }
            case NodeType_Paren: {
                return EvaluateInt(((node_paren*)Node)->Expression);
            }
            case NodeType_Negate: {
                return -EvaluateInt(((node_negate*)Node)->Expression);
            }
        }
        assert(0);
    }

    bool IsType(ast_type Type) {
        return Type == NodeType_TypeAlias || Type == NodeType_TypeDecl;
    }
    node_functiondecl* GetBinaryOpDeclByReturnType(node_type* Type, int OperationIdx) {
        for (auto Op : GlobalNode->BinaryOperators[OperationIdx]) {
            if (Equals(Op->ReturnType, Type)) {
                return Op;
            }
        }
        return 0;
    }
    template<class Comp>
    void GetOverloadMatchingArguments(std::vector<node_functiondecl*>& Result,
                                      const std::vector<node_functiondecl*>& Candidates,
                                      const std::vector<node_expression*>& Arguments,
                                      Comp&& Comparer) {
        for (auto Decl : Candidates) {
            if (Decl->Arguments.size() == Arguments.size()) {
                bool Match = true;
                for (int i=0; i < Decl->Arguments.size(); ++i) {
                    if (!ImplicitlyConvertible(Arguments[i], Decl->Arguments[i]->Type)) {
                        Match = false;
                        break;
                    }
                }
                if (Match) {
                    Result.push_back(Decl);
                }
            }
            else {
                bool HasDefaultArguments = false;
                for (auto Arg : Decl->Arguments)
                    HasDefaultArguments = HasDefaultArguments || Arg->InitialExpression;
                if (HasDefaultArguments) {
                    bool Match = true;
                    int i = 0;
                    for (; i < Min(Decl->Arguments.size(), Arguments.size()); ++i) {
                        if (!ImplicitlyConvertible(Arguments[i], Decl->Arguments[i]->Type)) {
                            Match = false;
                            break;
                        }
                    }
                    for (; i < Decl->Arguments.size(); ++i) {
                        if (!Decl->Arguments[i]->InitialExpression) {
                            Match = false;
                            break;
                        }
                    }
                    if (Match) {
                        Result.push_back(Decl);
                    }
                }
            }
        }
    }
#define GPFD_STRICT 1
#define GPFD_NO_ARGS 2
    int GetPossibleFunctionDecls(node_functioncall* Call, std::vector<node_functiondecl*>& Result, u32 Flags) {
        auto Search = [&](node* Where) {
            std::vector<node_functiondecl*> Candidates;
            for (node* TestNode : *GetChildren(Where)) {
                node_functiondecl* Decl = 0;
                if (TestNode->AstType == NodeType_FunctionDecl) {
                    Decl = (node_functiondecl*)TestNode;
                }
                else if (TestNode->AstType == NodeType_MemberFuncDecl) {
                    Decl = ((node_memberfuncdecl*)TestNode)->Declaration;
                }
                if (Decl) {
                    if (Decl->Identifier == Call->Identifier) {
                        Candidates.push_back(Decl);
                    }
                }
            }
            for (auto Decl : Candidates) {
                // HACK: Link all headers earlier
                Decl->Parent = Where;
                if (LinkHeader(Decl) != LINK_OK)
                    return LINK_FATAL;
                //////////
            }

            if (Flags & GPFD_NO_ARGS) {
                Result.insert(Result.end(), Candidates.begin(), Candidates.end());
                return LINK_OK;
            }

            if (Flags & GPFD_STRICT)
                GetOverloadMatchingArguments(Result, Candidates, Call->Arguments, Equals);
            else {
                bool (parser:: * fun)(node_expression*, node_expression*) = &parser::ImplicitlyConvertible;
                GetOverloadMatchingArguments(Result, Candidates, Call->Arguments, std::bind(this, fun));
            }

            return LINK_OK;
        };
        auto SearchScopeNode = GetSearchScopeNode();
        if (SearchScopeNode) {
            if (Search(SearchScopeNode) != LINK_OK)
                return LINK_FATAL;
        }
        else {
            node* SearchNode = GetNextFunctionHolder(Call);
            while (true) {
                if (Search(SearchNode) != LINK_OK)
                    return LINK_FATAL;
                SearchNode = GetNextFunctionHolder(SearchNode);
                if (!SearchNode) {
                    break;
                }
            }
        }
        return LINK_OK;
    }

    s32 LinkResult = 0;

    node_functiondecl* GetMatchingOverload(node_functioncall* Call) {
#if 0
        std::vector<node_functiondecl*> PossibleOverloads;
        if (GetPossibleFunctionDecls(Call, PossibleOverloads, 0) != LINK_OK) {
            LinkResult = LINK_FATAL;
            return 0;
        }
        if (PossibleOverloads.size() == 0) {
            PossibleOverloads.clear();
            if (GetPossibleFunctionDecls(Call, PossibleOverloads, GPFD_NO_ARGS) != LINK_OK) {
                LinkResult = LINK_FATAL;
                return 0;
            }
            PrintNotDeclaredError(Call->Location, MakeHeaderString(Call));
            if (PossibleOverloads.size() != 0) {
                ReportMessage("    Available overloads:");
                for (auto Decl : PossibleOverloads) {
                    ReportMessage("        ", MakeHeaderString(Decl));
                }
                ReportMessage();
            }
            LinkResult = LINK_FATAL;
            return 0;
        }
        else if (PossibleOverloads.size() > 1) {
            for (auto Decl : PossibleOverloads) {
                bool Match = true;
                for (int i=0; i < Call->Arguments.size(); ++i) {
                    if (Call->Arguments[i]->Flags & AstFlag_Known) {
                        if (IsInteger(Call->Arguments[i]->Type)) {
                            auto Eval = EvaluateInt(Call->Arguments[i]);
                            if (Eval < 0) {
                                for (auto Decl : PossibleOverloads) {
                                    if (IsSigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                        Call->Declaration = Decl;
                                        goto End;
                                    }
                                }
                            }
                            else {
                                for (auto Decl : PossibleOverloads) {
                                    if (IsUnsigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                        Call->Declaration = Decl;
                                        goto End;
                                    }
                                }
                                for (auto Decl : PossibleOverloads) {
                                    if (IsSigned(Decl->Arguments[i]->Type) && Fits(Eval, GetTypeDecl(Decl->Arguments[i]->Type))) {
                                        Call->Declaration = Decl;
                                        goto End;
                                    }
                                }
                            }
                            goto End;
                        }
                    }
                    if (!Equals(Call->Arguments[i]->Type, Decl->Arguments[i]->Type)) {
                        Match = false;
                        break;
                    }
                }
                if (Match) {
                    Call->Declaration = Decl;
                    break;
                }
            }
        End:
            if (!Call->Declaration) {
                ReportError(Call->Location, "Function '", Call->Identifier, "' is ambiguous");
                ReportMessage("    Matching overloads:");
                for (auto Decl : PossibleOverloads) {
                    ReportMessage("        ", MakeHeaderString(Decl));
                }
                ReportMessage();
                LinkResult = LINK_FATAL;
                return 0;
            }
        }
        else
            Call->Declaration = PossibleOverloads[0];
#endif
        std::vector<node_functiondecl*> Overloads;
        auto SearchInNode = [&](node* Where, auto&& Predicate) {
            for (node* TestNode : *GetChildren(Where)) {
                node_functiondecl* Decl = 0;
                if (TestNode->AstType == NodeType_FunctionDecl) {
                    Decl = (node_functiondecl*)TestNode;
                }
                else if (TestNode->AstType == NodeType_MemberFuncDecl) {
                    Decl = ((node_memberfuncdecl*)TestNode)->Declaration;
                }
                if (Decl) {
                    if (Predicate(Decl)) {
                        Overloads.push_back(Decl);
                    }
                }
            }
            //for (auto Decl : Candidates) {
            //    // HACK: Link all headers earlier
            //    Decl->Parent = Where;
            //    if (LinkHeader(Decl) != LINK_OK)
            //        return LINK_FATAL;
            //    //////////
            //}
        };
        auto IdentifierMatcher = [&](node_functiondecl* Decl) {
            return Decl->Identifier == Call->Identifier;
        };
        auto Matcher = [&](node_functiondecl* Decl) {
            if (Decl->Identifier != Call->Identifier)
                return false;
            if (Decl->Arguments.size() == Call->Arguments.size()) {
                if (LinkHeader(Decl) != LINK_OK)
                    assert(0);
                for (int i = 0; i < Call->Arguments.size(); ++i) {
                    if (!ImplicitlyConvertible(Call->Arguments[i], Decl->Arguments[i]->Type))
                        return false;
                }
                return true;
            }
            else if (Decl->Arguments.size() > Call->Arguments.size()) {
                if (HasDefaultArguments(Decl)) {
                    int i=0;
                    for (; i < Call->Arguments.size(); ++i) {
                        if (!ImplicitlyConvertible(Call->Arguments[i], Decl->Arguments[i]->Type))
                            return false;
                    }
                    for (; i < Decl->Arguments.size(); ++i) {
                        if (!Decl->Arguments[i]->InitialExpression)
                            return false;
                    }
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return false;
            }
        };
        auto SearchScopeNode = GetSearchScopeNode();
        auto Search = [&](auto&& Predicate) {
            if (SearchScopeNode) {
                SearchInNode(SearchScopeNode, Predicate);
            }
            else {
                node* SearchNode = GetNextFunctionHolder(Call);
                while (true) {
                    SearchInNode(SearchNode, Predicate);
                    SearchNode = GetNextFunctionHolder(SearchNode);
                    if (!SearchNode) {
                        break;
                    }
                }
            }
        };

        Search(Matcher);
        if (Overloads.size() == 0) {
            Search(IdentifierMatcher);
            PrintNotDeclaredError(Call->Location, MakeHeaderString(Call));
            if (Overloads.size() != 0) {
                ReportMessage("    Available overloads:");
                for (auto Decl : Overloads) {
                    ReportMessage("        ", MakeHeaderString(Decl));
                }
                ReportMessage();
            }
        }
        else if (Overloads.size() > 1) {
            ReportError(Call->Location, "Function '", Call->Identifier, "' is ambiguous");
            ReportMessage("    Matching overloads:");
            for (auto Decl : Overloads) {
                ReportMessage(message::column(48), Decl->Location, ": ", MakeHeaderString(Decl));
            }
            ReportMessage();
        }
        else
            return Overloads[0];
        return 0;
    }

    std::stack<node*> SearchScopeNodeStack;
    node* GetSearchScopeNode() {
        return SearchScopeNodeStack.size() ? SearchScopeNodeStack.top() : 0;
    }

    node_functiondecl* FindBinaryOpDecl(node_binaryop* BinaryOp) {
        // TODO: review
        for (auto Op : GlobalNode->BinaryOperators[ToInt(BinaryOp->Op)]) {
            if (GetTypeDecl(Op->Arguments[0]->Type) == GetTypeDecl(BinaryOp->FirstExpression) &&
                GetTypeDecl(Op->Arguments[1]->Type) == GetTypeDecl(BinaryOp->SecondExpression))
                return Op;
        }
        return 0;
    }
    node_typedecl* TryFindTypeDecl(string_view TypeIdentifier) {
        node_typedecl* Declaration = 0;
        scope* SearchScope = CurrentScope;
        while (SearchScope) {
            auto Iter = SearchScope->Types.find(TypeIdentifier);
            if (Iter != SearchScope->Types.end()) {
                Declaration = Iter->second;
            }
            SearchScope = SearchScope->Parent;
        }
        return Declaration;
    }
    node_typedecl* FindTypeDecl(location Location, string_view TypeIdentifier) {
        node_typedecl* Declaration = TryFindTypeDecl(TypeIdentifier);
        if (!Declaration) {
            ReportError(Location, "Type '", TypeIdentifier, "' is not declared");
        }
        return Declaration;
    }

    node_vardecl* TryFindVarDecl(node* ExprNode, string_view Identifier) {
        node_vardecl* Result = 0;
        auto Search = [&](std::vector<node*>& Declarations) {
            for (node* TestNode : Declarations) {
                if (TestNode->AstType == NodeType_VarDecl) {
                    node_vardecl& Var = *(node_vardecl*)TestNode;
                    if (Var.Identifier == Identifier) {
                        Result = &Var;
                        break;
                    }
                }
                else if (TestNode->AstType == NodeType_MemberVarDecl) {
                    node_vardecl& Var = *((node_membervardecl*)TestNode)->Declaration;
                    if (Var.Identifier == Identifier) {
                        Result = &Var;
                        break;
                    }
                }
            }
        };
        auto SearchScopeNode = GetSearchScopeNode();
        if (SearchScopeNode) {
        Retry:
            if (SearchScopeNode->AstType == NodeType_Type) {
                auto Type = (node_type*)SearchScopeNode;
                switch (Type->Type) {
                    case type::Array: {
                        if (Identifier == "begin" || Identifier == "end") {
                            return CreateVarDecl(Identifier, CreateTypePointer(((node_type_array*)Type)->Next));
                        }
                        break;
                    }
                    case type::Identifier:
                        Search(*GetChildren(Type));
                        break;
                    case type::Pointer:
                        SearchScopeNode = ((node_type_pointer*)Type)->Next;
                        goto Retry;
                }
            }
            else {
                Search(*GetChildren(SearchScopeNode));
            }
        }
        else {
            node* SearchNode = GetNextVariableHolder(ExprNode);
            while (SearchNode) {
                std::vector<node*>* Declarations = GetChildren(SearchNode);
                for (node* TestNode : *Declarations) {
                    if (TestNode == ExprNode) {
                        if (SearchNode->AstType == NodeType_FunctionDecl) // Not search for declarations after this statemtent
                            break;
                        else if (SearchNode->AstType == NodeType_Global || SearchNode->AstType == NodeType_NSDecl) // Do search for declarations after this statemtent
                            continue;
                        else
                            assert(false);// Variable can be declared only in function or global scope
                    }
                    if (TestNode->AstType == NodeType_VarDecl) {
                        node_vardecl& Var = *(node_vardecl*)TestNode;
                        if (Var.Identifier == Identifier) {
                            Result = &Var;
                            break;
                        }
                    }
                    else if (TestNode->AstType == NodeType_MemberVarDecl) {
                        node_vardecl& Var = *((node_membervardecl*)TestNode)->Declaration;
                        if (Var.Identifier == Identifier) {
                            Result = &Var;
                            break;
                        }
                    }
                }
                if (SearchNode->AstType == NodeType_FunctionDecl) {
                    node_functiondecl& FuncDecl = *(node_functiondecl*)SearchNode;
                    for (node_vardecl* Arg : FuncDecl.Arguments) {
                        if (Arg->Identifier == Identifier) {
                            Result = Arg;
                        }
                    }
                }
                if (Result) {
                    if (ExprNode->AstType == NodeType_FunctionDecl && SearchNode->AstType == NodeType_FunctionDecl) {
                        return 0;
                        //ReportError(Location, "Can't access variable declared in another function's scope");
                    }
                    break;
                }
                SearchNode = GetNextVariableHolder(SearchNode);
            }
        }
        return Result;
    }
    node_vardecl* FindVarDecl(node* ExprNode, location Location, string_view Identifier) {
        auto Result = TryFindVarDecl(ExprNode, Identifier);
        if (!Result) {
            ReportError(Location, "Variable '", Identifier, "' is not declared");
        }
        return Result;
    }
    node_functiondecl* TryFindFunctionDecl(node* ExprNode, string_view Identifier) {
        node_functiondecl* Result = 0;
        auto SearchScopeNode = GetSearchScopeNode();
        if (SearchScopeNode) {
            std::vector<node*>& Declarations = *GetChildren(SearchScopeNode);
            for (node* Node : Declarations) {
                if (Node->AstType == NodeType_FunctionDecl) {
                    node_functiondecl* Decl = (node_functiondecl*)Node;
                    if (Decl->Identifier == Identifier) {
                        Result = (node_functiondecl*)Node;
                        break;
                    }
                }
            }
        }
        else {
            node* SearchNode = GetNextFunctionHolder(ExprNode);
            while (SearchNode) {
                std::vector<node*>& Declarations = *GetChildren(SearchNode);
                for (node* Node : Declarations) {
                    if (Node->AstType == NodeType_FunctionDecl) {
                        node_functiondecl* Decl = (node_functiondecl*)Node;
                        if (Decl->Identifier == Identifier) {
                            Result = (node_functiondecl*)Node;
                            break;
                        }
                    }
                }
                if (Result)
                    break;
                SearchNode = GetNextFunctionHolder(SearchNode);
            }
        }
        return Result;
    }
    node_nsdecl* TryFindNamespaceDecl(node* ExprNode, string_view Identifier) {
        node_nsdecl* Result = 0;
        node* SearchNode = GetNextNamespaceHolder(ExprNode);
        while (SearchNode) {
            std::vector<node*>& Declarations = *GetChildren(SearchNode);
            for (node* Node : Declarations) {
                if (Node->AstType == NodeType_NSDecl) {
                    node_nsdecl* NSDecl = (node_nsdecl*)Node;
                    if (NSDecl->Identifier == Identifier) {
                        Result = (node_nsdecl*)Node;
                        break;
                    }
                }
            }
            if (Result)
                break;
            SearchNode = GetNextNamespaceHolder(SearchNode);
        }
        return Result;
    }
    node_nsdecl* FindNamespaceDecl(node* ExprNode, location Location, string_view Identifier) {
        auto Result = TryFindNamespaceDecl(ExprNode, Identifier);
        if (!Result) {
            ReportError(Location, "Namespace '", Identifier, "' is not declared");
        }
        return Result;
    }

    std::string MakeHeaderString(node_functioncall* Call) {
        string_builder Builder;
        Builder.Append(Call->Identifier);
        Builder.Append('(');
        for (auto Arg : Call->Arguments) {
            Builder.Append(ToString(GetType(Arg)));
            if (Arg != Call->Arguments.back())
                Builder.Append(", ");
        }
        Builder.Append(')');
        return Builder.GetString();
    }
    std::string MakeHeaderString(node_functiondecl* Decl) {
        string_builder Builder;
        Builder.Append(Decl->Identifier);
        Builder.Append('(');
        for (auto Arg : Decl->Arguments) {
            Builder.Append(ToString(Arg->Type));
            if (Arg != Decl->Arguments.back())
                Builder.Append(", ");
        }
        Builder.Append("): ");
        Builder.Append(ToString(Decl->ReturnType));
        return Builder.GetString();
    }

    bool EnsureValid(node_type* Type);

    void PrintNotDeclaredError(location Location, string_view Identifier);

    struct found_import {
        FILE* File;
        char FullPath[256];
    };

    found_import FindImportFile(string_view Name);

    node_typedecl* Type_void = 0;
    node_typedecl* Type_bool = 0;
    node_typedecl* Type_int8 = 0;
    node_typedecl* Type_int16 = 0;
    node_typedecl* Type_int32 = 0;
    node_typedecl* Type_int64 = 0;
    node_typedecl* Type_uint8 = 0;
    node_typedecl* Type_uint16 = 0;
    node_typedecl* Type_uint32 = 0;
    node_typedecl* Type_uint64 = 0;
    node_typedecl* Type_float32 = 0;
    node_typedecl* Type_float64 = 0;
    node_typedecl* Type_char = 0;
    node_typedecl* Type_string = 0;

    node_type_identifier* TypeIdentifier_void = 0;
    node_type_identifier* TypeIdentifier_bool = 0;
    node_type_identifier* TypeIdentifier_int8 = 0;
    node_type_identifier* TypeIdentifier_int16 = 0;
    node_type_identifier* TypeIdentifier_int32 = 0;
    node_type_identifier* TypeIdentifier_int64 = 0;
    node_type_identifier* TypeIdentifier_uint8 = 0;
    node_type_identifier* TypeIdentifier_uint16 = 0;
    node_type_identifier* TypeIdentifier_uint32 = 0;
    node_type_identifier* TypeIdentifier_uint64 = 0;
    node_type_identifier* TypeIdentifier_float32 = 0;
    node_type_identifier* TypeIdentifier_float64 = 0;
    node_type_identifier* TypeIdentifier_char = 0;
    node_type_identifier* TypeIdentifier_string = 0;
    node_type_pointer* TypePtr_char = 0;

    // These types are of size that can hold any pointer difference, they vary depending on target architecture
    node_type_identifier* TypeIdentifier_int = 0;
    node_type_identifier* TypeIdentifier_uint = 0;

    u64 GetSizeOf(node_type* Type) {
        switch (Type->Type) {
            case type::Array: {
                auto Arr = (node_type_array*)Type;
                return GetSizeOf(Arr->Next) * Arr->Count;
            }
            case type::Identifier:
                return ((node_type_identifier*)Type)->Decl->Size;
            case type::Pointer:
                return TargetArch.PtrSize;
        }
        assert(0);
        return 0;
    }
    u64 GetAlign(node_type* Type) {
        switch (Type->Type) {
            case type::Array: {
                auto Arr = (node_type_array*)Type;
                return GetAlign(Arr->Next);
            }
            case type::Identifier:
                return ((node_type_identifier*)Type)->Decl->Align.Value;
            case type::Pointer:
                return TargetArch.PtrSize;
        }
        assert(0);
        return 0;
    }

    struct int_range {
        s64 Min;
        u64 Max;
    };
    int_range GetIntRange(node_typedecl* Type) {
        if (Type == Type_int8)   return {INT8_MIN ,INT8_MAX};
        if (Type == Type_int16)  return {INT16_MIN,INT16_MAX};
        if (Type == Type_int32)  return {INT32_MIN,INT32_MAX};
        if (Type == Type_int64)  return {INT64_MIN,INT64_MAX};
        if (Type == Type_uint8)  return {0,UINT8_MAX};
        if (Type == Type_uint16) return {0,UINT16_MAX};
        if (Type == Type_uint32) return {0,UINT32_MAX};
        if (Type == Type_uint64) return {0,UINT64_MAX};
        assert(0);
        return {};
    }

    bool Fits(node_typedecl* Src, node_typedecl* Dst) {
        auto S = GetIntRange(Src);
        auto D = GetIntRange(Dst);
        return D.Min <= S.Min && S.Max <= D.Max;
    }
    bool Fits(s64 Value, node_typedecl* Dst) {
        if (Value < 0 && IsUnsigned(Dst))
            return false;
        auto D = GetIntRange(Dst);
        return D.Min <= Value && Value <= D.Max;
    }

    inline bool IsPointer (node_type* Type) { return Type->Type == type::Pointer; }
    inline bool IsArray   (node_type* Type) { return Type->Type == type::Array; }
    inline bool IsInteger (node_type* Type) { return Type->Type == type::Identifier && IsInteger (((node_type_identifier*)Type)->Decl); }
    inline bool IsSigned  (node_type* Type) { return Type->Type == type::Identifier && IsSigned  (((node_type_identifier*)Type)->Decl); }
    inline bool IsUnsigned(node_type* Type) { return Type->Type == type::Identifier && IsUnsigned(((node_type_identifier*)Type)->Decl); }
    inline bool IsFloat   (node_type* Type) { return Type->Type == type::Identifier && IsFloat   (((node_type_identifier*)Type)->Decl); }
    inline bool IsNumber  (node_type* Type) { return Type->Type == type::Identifier && IsNumber  (((node_type_identifier*)Type)->Decl); }
    inline bool IsBuiltIn (node_type* Type) {
        return
            Type->Type == type::Pointer ||
            Type->Type == type::Array ||
            Type->Type == type::Identifier && IsBuiltIn (((node_type_identifier*)Type)->Decl);
    }

    inline bool IsInteger (node_type_identifier* Type) { return IsInteger (Type->Decl); }
    inline bool IsSigned  (node_type_identifier* Type) { return IsSigned  (Type->Decl); }
    inline bool IsUnsigned(node_type_identifier* Type) { return IsUnsigned(Type->Decl); }
    inline bool IsFloat   (node_type_identifier* Type) { return IsFloat   (Type->Decl); }
    inline bool IsNumber  (node_type_identifier* Type) { return IsNumber  (Type->Decl); }
    inline bool IsBuiltIn (node_type_identifier* Type) { return IsBuiltIn (Type->Decl); }

    inline bool IsPointer (node_expression* Expr) { return IsPointer (Expr->Type); }
    inline bool IsArray   (node_expression* Expr) { return IsArray   (Expr->Type); }
    inline bool IsInteger (node_expression* Expr) { return IsInteger (Expr->Type); }
    inline bool IsSigned  (node_expression* Expr) { return IsSigned  (Expr->Type); }
    inline bool IsUnsigned(node_expression* Expr) { return IsUnsigned(Expr->Type); }
    inline bool IsFloat   (node_expression* Expr) { return IsFloat   (Expr->Type); }
    inline bool IsNumber  (node_expression* Expr) { return IsNumber  (Expr->Type); }
    inline bool IsBuiltIn (node_expression* Expr) { return IsBuiltIn (Expr->Type); }

    inline bool IsInteger (node_typedecl* Type) {
        return
            Type == Type_int8 ||
            Type == Type_int16 ||
            Type == Type_int32 ||
            Type == Type_int64 ||
            Type == Type_uint8 ||
            Type == Type_uint16 ||
            Type == Type_uint32 ||
            Type == Type_uint64;
    }
    inline bool IsSigned  (node_typedecl* Type) {
        return
            Type == Type_int8 ||
            Type == Type_int16 ||
            Type == Type_int32 ||
            Type == Type_int64;
    }
    inline bool IsUnsigned(node_typedecl* Type) {
        return
            Type == Type_uint8 ||
            Type == Type_uint16 ||
            Type == Type_uint32 ||
            Type == Type_uint64;
    }
    inline bool IsFloat   (node_typedecl* Type) {
        return
            Type == Type_float32 ||
            Type == Type_float64;
    }
    inline bool IsNumber  (node_typedecl* Type) {
        return IsInteger(Type) || IsFloat(Type);
    }
    inline bool IsBuiltIn (node_typedecl* Type) {
        return
            Type == Type_void ||
            Type == Type_bool ||
            Type == Type_int8 ||
            Type == Type_int16 ||
            Type == Type_int32 ||
            Type == Type_int64 ||
            Type == Type_uint8 ||
            Type == Type_uint16 ||
            Type == Type_uint32 ||
            Type == Type_uint64 ||
            Type == Type_float32 ||
            Type == Type_float64 ||
            Type == Type_char ||
            Type == Type_string;
    }
#define NCR_DOES_NOT_FIT 1
    u32 NotConvertibleReason = 0;
    s64 EvaluatedValue;
    inline bool ImplicitlyConvertible(node_expression* Src, node_expression* Dst) {
        return ImplicitlyConvertible(Src->Type, Dst->Type, Src);
    }
    inline bool ImplicitlyConvertible(node_expression* Src, node_type* DstType) {
        return ImplicitlyConvertible(Src->Type, DstType, Src);
    }
    inline bool ImplicitlyConvertible(node_type* SrcType, node_type* DstType, node* SrcNode) {
        NotConvertibleReason = 0;
        if (SrcType->Type == type::Identifier && DstType->Type == type::Identifier) {
            auto Src = (node_type_identifier*)SrcType;
            auto Dst = (node_type_identifier*)DstType;
            if (Src->Decl == Dst->Decl) {
                return true;
            }
            else {
                if (SrcNode->Flags & AstFlag_Known) {
                    if (IsInteger(Dst->Decl) && IsInteger(Src->Decl)) {
                        EvaluatedValue = EvaluateInt(SrcNode);
                        if (Fits(EvaluatedValue, Dst->Decl))
                            return true;
                        else {
                            NotConvertibleReason = NCR_DOES_NOT_FIT;
                            return false;
                        }
                    }
                    else {
                        return false;
                    }
                }
                else {
                    if (IsInteger(Src) && IsInteger(Dst)) {
                        if (Fits(Src->Decl, Dst->Decl))
                            return true;
                        else {
                            NotConvertibleReason = NCR_DOES_NOT_FIT;
                            return false;
                        }
                    }
                    else {
                        return false;
                    }
                }
            }
        }
        else {
            if (IsPointer(DstType)) {
                auto DstPtr = (node_type_pointer*)DstType;
                if (IsInteger(SrcType)) {
                    return SrcNode->AstType == NodeType_Literal && ((node_literal*)SrcNode)->Value.Int == 0;
                }
                else if (IsPointer(SrcType)) {
                    return Equals(DstPtr->Next, TypeIdentifier_void) || Equals(SrcType, DstType);
                }
                else {
                    return false;
                }
            }
            else {
                return Equals(SrcType, DstType);
            }
        }
    }
    inline bool EnsureImplicitlyConvertible(node_expression* Src, node_expression* Dst, location Location) {
        return EnsureImplicitlyConvertible(Src->Type, Dst->Type, Src, Location);
    }
    inline bool EnsureImplicitlyConvertible(node_expression* Src, node_type* DstType, location Location) {
        return EnsureImplicitlyConvertible(Src->Type, DstType, Src, Location);
    }
    inline bool EnsureImplicitlyConvertible(node_type* SrcType, node_type* DstType, node* SrcNode, location Location) {
        if (ImplicitlyConvertible(SrcType, DstType, SrcNode)) {
            return true;
        }
        switch (NotConvertibleReason) {
            case NCR_DOES_NOT_FIT:
                ReportError(Location, "Can't convert from '", SrcType, "' to '", DstType, "'. Evaluated value '", EvaluatedValue, "' does not fit into destination range");
                break;
            default:
                ReportError(Location, "Can't convert from '", SrcType, "' to '", DstType, "'");
                break;
        }
        return false;
    }
    inline bool IsIdentifierValid(string_view Identifier) {
        return
            Identifier != "sizeof" &&
            Identifier != "type";
    }
private:
    static void FreeScope(scope* Scope) {
        for (scope* Ch : Scope->Children) {
            FreeScope(Ch);
        }
        delete Scope;
    }
};
