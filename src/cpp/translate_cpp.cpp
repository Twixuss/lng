#include "..\lng\translate.h"
#include "..\lng\parser.h"
int Append(string_builder* Builder, node* Node);

auto fputs(FILE* File, const char* Str) {
    return fputs(Str, File);
}

string_builder MemberFunctionDefinitions;

u32 IndentLevel = 0;
void AppendIndent(string_builder* Builder) {
    for (u32 i=0; i < IndentLevel; ++i) {
        Builder->Append("    ");
    }
}
void AppendIdentifier(string_builder* Builder, node* Node) {
    Builder->Append(GetIdentifier(Node));
    node* Addr = [&]() -> node* {
        switch (Node->AstType) {
            //case NodeType_MemberVarDecl: return ((node_membervardecl*)Node)->Declaration;
            case NodeType_Identifier: {
                auto Ident = (node_identifier*)Node;
                switch (Ident->Declaration->AstType) {
                    case NodeType_NSDecl:
                    case NodeType_VarDecl:
                        return 0;
                }
                return Ident->Declaration;
            }
            case NodeType_FunctionCall: {
                auto Call = (node_functioncall*)Node;
                if (Call->Declaration->IsInternal)
                    return 0;
                return Call->Declaration;
            }
            case NodeType_FunctionDecl:
            //case NodeType_TypeDecl:
                //case NodeType_VarDecl:
                return Node;
            default: return 0;
        }
    }();
    if (Addr) {
        Builder->Append('_');
        string_view DigitTable = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäå¸æçèéêëìíîïðñòóôõö÷øùúûüýþÿ";
        Builder->Append(Addr, DigitTable.Count(), false, DigitTable);
    }
}
void AppendParent(string_builder* Builder, node* Node, node* Declaration) {
    if (Node->Parent->AstType == NodeType_MemberAccess || (Declaration->Parent->AstType != NodeType_NSDecl && Declaration->Parent->AstType != NodeType_Global))
        return;
    std::vector<node*> Branch;
    auto It = Declaration->Parent;
    while (It) {
        Branch.push_back(It);
        It = It->Parent;
    }
    //if (Declaration->Parent->AstType == NodeType_Global) {
    Builder->Append("::");
    //}
    //else {
    for (auto B : Reverse(Branch)) {

        switch (B->AstType) {
            case NodeType_NSDecl:
                AppendIdentifier(Builder, B);
                break;
            default:
                continue;
        }
        switch (B->AstType) {
            case NodeType_NSDecl:
                Builder->Append("::");
                break;
            default:
                break;
        }
    }
    //}
}
#define TYPEDECL_DEFINED_BIT (1 << 0)
void AppendType(string_builder* Builder, node_type* Type) {
    std::vector<node_type*> Types;

    while (1) {
        Types.push_back(Type);
        if (Type->Type == type::Identifier)
            break;
        else if (Type->Type == type::Pointer)
            Type = ((node_type_pointer*)Type)->Next;
        else if (Type->Type == type::Array)
            break;
        else
            assert(0);
    }

    for (auto Type : Reverse(Types)) {
        switch (Type->Type) {
            case type::Array: {
                auto Arr = (node_type_array*)Type;
                Builder->Append("array<");
                AppendType(Builder, Arr->Next);
                Builder->Append(", ");
                Builder->Append(std::to_string(Arr->Count));
                Builder->Append('>');
                continue;
            }
            case type::Identifier: {
                Builder->Append(((node_type_identifier*)Type)->Decl->Identifier);
                continue;
            }
            case type::Pointer: {
                Builder->Append('*');
                continue;
            }
        }
        assert(0);
    }
}

int AppendTypeDefinition(parser& Parser, string_builder& Definitions, node_typedecl* Type) {
    if (Parser.IsBuiltIn(Type) || (Type->CustomData & TYPEDECL_DEFINED_BIT))
        return TRANSLATE_OK;
    string_builder Builder;
    bool UseUnions = false;
    for (auto Member : Type->Members) {
        if (Member->AstType == NodeType_MemberVarDecl && ((node_membervardecl*)Member)->Offset.Custom)
            UseUnions = true;
    }
    if (UseUnions)
        Builder.Append("union ");
    else
        Builder.Append("struct ");
    AppendIdentifier(&Builder, Type);
    Builder.Append(" alignas(");
    Builder.Append(Type->Align.Value);
    Builder.Append(") {\n");
    u32 PaddingIndex = 0;
    for (auto Member : Type->Members) {
        switch (Member->AstType) {
            case NodeType_MemberVarDecl: {
                auto MemberVar = (node_membervardecl*)Member;
                if (UseUnions) {
                    auto MemberTypeDecl = Parser.GetTypeDecl(MemberVar->Declaration->Type);
                    if (!(MemberTypeDecl->CustomData & TYPEDECL_DEFINED_BIT)) {
                        if (AppendTypeDefinition(Parser, Definitions, MemberTypeDecl) != TRANSLATE_OK)
                            return TRANSLATE_ERROR;
                    }
                    if (MemberVar->Offset.Value) {
                        Builder.Append("    struct {\n        char __Padding");
                        Builder.Append(PaddingIndex++);
                        Builder.Append("[");
                        Builder.Append(MemberVar->Offset.Value);
                        Builder.Append("];\n        ");
                        AppendType(&Builder, MemberVar->Declaration->Type);
                        Builder.Append(" ");
                        Builder.Append(MemberVar->Declaration->Identifier);
                        Builder.Append(";\n    };\n");
                    }
                    else {
                        Builder.Append("    ");
                        AppendType(&Builder, MemberVar->Declaration->Type);
                        Builder.Append(" ");
                        Builder.Append(MemberVar->Declaration->Identifier);
                        Builder.Append(";\n");
                    }
                }
                else {
                    Builder.Append("    ");
                    AppendType(&Builder, MemberVar->Declaration->Type);
                    Builder.Append(" ");
                    Builder.Append(MemberVar->Declaration->Identifier);
                    Builder.Append(";\n");
                }
                break;
            }
            case NodeType_MemberFuncDecl: {
                auto MemberFn = (node_memberfuncdecl*)Member;
                assert(!MemberFn->Declaration->IsInternal);
                Builder.Append("    ");
                AppendType(&Builder, MemberFn->Declaration->ReturnType);
                Builder.Append(" ");
                AppendIdentifier(&Builder, MemberFn->Declaration);
                Builder.Append("(");
                for (auto Arg : MemberFn->Declaration->Arguments) {
                    AppendType(&Builder, Arg->Type);
                    if (Arg != MemberFn->Declaration->Arguments.back())
                        Builder.Append(",");
                }
                Builder.Append(");\n");

                Append(0, MemberFn->Declaration);
                break;
            }
            default:
                assert(0);
        }
    }
    Type->CustomData |= TYPEDECL_DEFINED_BIT;
    Builder.Append("};\n");
    Definitions.Append(Builder.GetString());
    return TRANSLATE_OK;
};
compiler* Compiler = 0;
string_builder FuncHeaderBuilder, FuncBuilder, VarsBuilder;
std::unordered_map<string_view, string_view> InternalFunctions;
int Append(string_builder* Builder, node_assignment* Assignment) {
    if (Append(Builder, Assignment->Assigned) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    Builder->Append(" = ");
    if (Append(Builder, Assignment->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_vardecl* VarDecl) {
    AppendType(Builder, VarDecl->Type);
    Builder->Append(' ');
    AppendIdentifier(Builder, VarDecl);
    if (VarDecl->InitialExpression) {
        Builder->Append(" = ");
        if (Append(Builder, VarDecl->InitialExpression) != TRANSLATE_OK)
            return TRANSLATE_ERROR;
    }
    else {
        Builder->Append(" = {}");
    }
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_functiondecl* FunctionDecl) {
    if (FunctionDecl->IsInternal) {
        std::vector<string_view> Identifiers;
        for (node* N = FunctionDecl->Parent; N && N->AstType != NodeType_Global; N = N->Parent) {
            switch (N->AstType) {
                case NodeType_NSDecl: Identifiers.push_back(((node_nsdecl*)N)->Identifier); continue;
            }
            ReportError(FunctionDecl->Location, "Internal function can't be defined here");
            return TRANSLATE_ERROR;
        }
        string_builder InternalIDBuilder;
        for (auto Id : Reverse(Identifiers)) {
            InternalIDBuilder.Append(Id);
            InternalIDBuilder.Append('.');
        }
        InternalIDBuilder.Append(FunctionDecl->Identifier);
        InternalIDBuilder.Append("(");
        for (auto Arg : FunctionDecl->Arguments) {
            AppendType(&InternalIDBuilder, Arg->Type);
            if (Arg != FunctionDecl->Arguments.back())
                InternalIDBuilder.Append(",");
        }
        InternalIDBuilder.Append(")");
        auto It = InternalFunctions.find(InternalIDBuilder.GetString());
        if (It == InternalFunctions.end()) {
            ReportError(FunctionDecl->Location, "No such internal function");
            return TRANSLATE_ERROR;
        }
        else {
            auto Fn = It->second;
            FuncHeaderBuilder.Append(Fn);
            return TRANSLATE_OK;
        }
    }
    else {
        if (FunctionDecl->Parent->AstType != NodeType_MemberFuncDecl) {
            AppendIndent(&FuncHeaderBuilder);
            AppendType(&FuncHeaderBuilder, FunctionDecl->ReturnType);
            FuncHeaderBuilder.Append(' ');
            if (FunctionDecl->Parent->AstType == NodeType_Global && FunctionDecl->Identifier == "main") {
                FuncHeaderBuilder.Append("main");
            }
            else {
                AppendIdentifier(&FuncHeaderBuilder, FunctionDecl);
            }
            FuncHeaderBuilder.Append('(');
            for (auto Arg : FunctionDecl->Arguments) {
                AppendType(&FuncHeaderBuilder, Arg->Type);
                FuncHeaderBuilder.Append(' ');
                AppendIdentifier(&FuncHeaderBuilder, Arg);
                if (Arg != FunctionDecl->Arguments.back()) {
                    FuncHeaderBuilder.Append(", ");
                }
            }
            FuncHeaderBuilder.Append(");\n");
            for (auto Stm : FunctionDecl->Statements) {
                if (Stm->AstType == NodeType_FunctionDecl) {
                    if (Append(Builder, (node_functiondecl*)Stm) != TRANSLATE_OK)
                        return TRANSLATE_ERROR;
                }
            }
        }


        if (!Builder)
            Builder = &FuncBuilder;
        AppendIndent(Builder);
        AppendType(Builder, FunctionDecl->ReturnType);
        Builder->Append(' ');
        if (FunctionDecl->Parent->AstType == NodeType_MemberFuncDecl) {
            Builder->Append(((node_typedecl*)FunctionDecl->Parent->Parent)->Identifier);
            Builder->Append("::");
        }
        if (FunctionDecl->Parent->AstType == NodeType_Global && FunctionDecl->Identifier == "main") {
            Builder->Append("main");
        }
        else {
            AppendIdentifier(Builder, FunctionDecl);
        }
        Builder->Append('(');
        for (auto Arg : FunctionDecl->Arguments) {
            AppendType(Builder, Arg->Type);
            Builder->Append(' ');
            AppendIdentifier(Builder, Arg);
            if (Arg != FunctionDecl->Arguments.back()) {
                Builder->Append(", ");
            }
        }
        Builder->Append(") {\n");
        ++IndentLevel;
        for (auto S : FunctionDecl->Statements) {
            if (S->AstType != NodeType_TypeAlias && S->AstType != NodeType_TypeDecl && S->AstType != NodeType_FunctionDecl) {
                AppendIndent(Builder);
                if (Append(Builder, S) != TRANSLATE_OK)
                    return TRANSLATE_ERROR;
                if (S->AstType != NodeType_If && S->AstType != NodeType_While && S->AstType != NodeType_Else)
                    Builder->Append(";\n");
            }
        }
        --IndentLevel;
        AppendIndent(Builder);
        Builder->Append("}\n");
        return TRANSLATE_OK;
    }
}
int Append(string_builder* Builder, node_typealias* TypeAlias) {
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_identifier* Identifier) {
    AppendParent(Builder, Identifier, Identifier->Declaration);
    AppendIdentifier(Builder, Identifier);
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_functioncall* Call) {
    if (Call->Parent->AstType != NodeType_MemberAccess)
        AppendParent(Builder, Call, Call->Declaration);
    AppendIdentifier(Builder, Call);
    Builder->Append('(');
    if (Call->Arguments.size() != Call->Declaration->Arguments.size()) {
        for (auto Arg : Call->Arguments) {
            if (Append(Builder, Arg) != TRANSLATE_OK)
                return TRANSLATE_ERROR;
            Builder->Append(", ");
        }
        for (int i=Call->Arguments.size(); i < Call->Declaration->Arguments.size(); ++i) {
            if (Append(Builder, Call->Declaration->Arguments[i]->InitialExpression) != TRANSLATE_OK)
                return TRANSLATE_ERROR;
            if (i < Call->Declaration->Arguments.size() - 1) {
                Builder->Append(", ");
            }
        }
    }
    else {
        for (auto Arg : Call->Arguments) {
            if (Append(Builder, Arg) != TRANSLATE_OK)
                return TRANSLATE_ERROR;
            if (Arg != Call->Arguments.back()) {
                Builder->Append(", ");
            }
        }
    }
    Builder->Append(')');
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_memberaccess* MemberAccess) {
    if (Append(Builder, MemberAccess->First) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    if (MemberAccess->First->AstType == NodeType_Literal) {
        Builder->Append("_str.");
    }
    else {
        switch (Compiler->Parser->GetDeclaration(MemberAccess->First)->AstType) {
            case NodeType_NSDecl:
                Builder->Append("::");
                break;
            case NodeType_VarDecl:
                Builder->Append('.');
                break;
            default:
                assert(0);
        }
    }
    if (Append(Builder, MemberAccess->Second) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_sizeof* Sizeof) {
    Builder->Append(Compiler->Parser->GetSizeOf(Sizeof->TestType));
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_paren* Paren) {
    Builder->Append('(');
    if (Append(Builder, Paren->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    Builder->Append(')');
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_literal* Literal) {
    if (Literal->LiteralType == data_type::String) {
        Builder->Append('"');
        Builder->Append(Literal->Value.Str);
        Builder->Append('"');
    }
    else if (Literal->LiteralType == data_type::Char) {
        Builder->Append('\'');
        Builder->Append(Literal->Value.Str);
        Builder->Append('\'');
    }
    else if (Literal->LiteralType == data_type::UInt) {
        Builder->Append(Literal->Value.Int);
    }
    else {
        Builder->Append(Literal->Value.Str);
    }
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_binaryop* BinaryOp) {
    Builder->Append('(');
    if (Append(Builder, BinaryOp->FirstExpression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    Builder->Append(Compiler->Parser->BinopString(BinaryOp->Op));
    if (Append(Builder, BinaryOp->SecondExpression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    Builder->Append(')');
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_return* Return) {
    Builder->Append("return ");
    if (Append(Builder, Return->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_nsdecl* NSDecl) {
    auto Begin = [](string_builder* Builder, node_nsdecl* NSDecl) {
        AppendIndent(Builder);
        Builder->Append("namespace ");
        AppendIdentifier(Builder, NSDecl);
        Builder->Append(" {\n");
    };
    auto End = [](string_builder* Builder) {
        AppendIndent(Builder);
        Builder->Append("}\n");
    };
    Begin(&VarsBuilder, NSDecl);
    Begin(&FuncHeaderBuilder, NSDecl);
    Begin(&FuncBuilder, NSDecl);
    ++IndentLevel;
    for (auto Decl : NSDecl->Declarations) {
        if (Decl->AstType == NodeType_VarDecl) {
            AppendIndent(&FuncBuilder);
            if (Append(Builder, Decl) != TRANSLATE_OK)
                return TRANSLATE_ERROR;
            FuncBuilder.Append(";\n");
        }
        else {
            if (Append(Builder, Decl) != TRANSLATE_OK)
                return TRANSLATE_ERROR;
        }
    }
    --IndentLevel;
    End(&VarsBuilder);
    End(&FuncHeaderBuilder);
    End(&FuncBuilder);
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_if* If) {
    FuncBuilder.Append("if(");
    if (Append(&FuncBuilder, If->Condition) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(") {\n");
    ++IndentLevel;
    for (auto S : If->Statements) {
        AppendIndent(&FuncBuilder);
        if (Append(&FuncBuilder, S) != TRANSLATE_OK)
            return TRANSLATE_ERROR;
        if (S->AstType != NodeType_If && S->AstType != NodeType_While && S->AstType != NodeType_Else)
            FuncBuilder.Append(";\n");
    }
    --IndentLevel;
    AppendIndent(&FuncBuilder);
    FuncBuilder.Append("}\n");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_else* Else) {
    FuncBuilder.Append("else {\n");
    ++IndentLevel;
    for (auto S : Else->Statements) {
        AppendIndent(&FuncBuilder);
        if (Append(&FuncBuilder, S) != TRANSLATE_OK)
            return TRANSLATE_ERROR;
        if (S->AstType != NodeType_If && S->AstType != NodeType_While && S->AstType != NodeType_Else)
            FuncBuilder.Append(";\n");
    }
    --IndentLevel;
    AppendIndent(&FuncBuilder);
    FuncBuilder.Append("}\n");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_while* While) {
    FuncBuilder.Append("while(");
    if (Append(&FuncBuilder, While->Condition) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(") {\n");
    ++IndentLevel;
    for (auto S : While->Statements) {
        AppendIndent(&FuncBuilder);
        if (Append(&FuncBuilder, S) != TRANSLATE_OK)
            return TRANSLATE_ERROR;
        if (S->AstType != NodeType_If && S->AstType != NodeType_While && S->AstType != NodeType_Else)
            FuncBuilder.Append(";\n");
    }
    --IndentLevel;
    AppendIndent(&FuncBuilder);
    FuncBuilder.Append("}\n");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_address* Address) {
    FuncBuilder.Append("(&");
    if (Append(&FuncBuilder, Address->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(")");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_deref* Deref) {
    FuncBuilder.Append("(*");
    if (Append(&FuncBuilder, Deref->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(")");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_index* Index) {
    if (Append(&FuncBuilder, Index->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append('[');
    if (Append(&FuncBuilder, Index->IndexExpression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(']');
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_negate* Negate) {
    FuncBuilder.Append("(-");
    if (Append(&FuncBuilder, Negate->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(")");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_not* Not) {
    FuncBuilder.Append("(!");
    if (Append(&FuncBuilder, Not->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(")");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_as* As) {
    FuncBuilder.Append("((");
    AppendType(&FuncBuilder, As->Type);
    FuncBuilder.Append(")");
    if (Append(&FuncBuilder, As->Expression) != TRANSLATE_OK)
        return TRANSLATE_ERROR;
    FuncBuilder.Append(")");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_break* Break) {
    FuncBuilder.Append("break");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node_continue* Continue) {
    FuncBuilder.Append("continue");
    return TRANSLATE_OK;
}
int Append(string_builder* Builder, node* Node) {
    switch (Node->AstType) {
        case NodeType_Literal:      return Append(Builder, (node_literal*)Node);
        case NodeType_FunctionDecl: return Append(Builder, (node_functiondecl*)Node);
        case NodeType_VarDecl: {
            switch (Node->Parent->AstType) {
                case NodeType_Global:
                    return Append(&VarsBuilder, (node_vardecl*)Node);
                default:
                    return Append(&FuncBuilder, (node_vardecl*)Node);
            }
        }
        case NodeType_Assignment:   return Append(Builder, (node_assignment*)Node);
        case NodeType_FunctionCall: return Append(Builder, (node_functioncall*)Node);
        case NodeType_TypeDecl:     AppendIdentifier(Builder, (node_typedecl*)Node); return TRANSLATE_OK;
        case NodeType_TypeAlias:    return Append(Builder, (node_typealias*)Node);
        case NodeType_Identifier:   return Append(Builder, (node_identifier*)Node);
        case NodeType_MemberAccess: return Append(Builder, (node_memberaccess*)Node);
        case NodeType_Sizeof:       return Append(Builder, (node_sizeof*)Node);
        case NodeType_Paren:        return Append(Builder, (node_paren*)Node);
        case NodeType_BinaryOp:     return Append(Builder, (node_binaryop*)Node);
        case NodeType_Return:       return Append(Builder, (node_return*)Node);
        case NodeType_NSDecl:       return Append(Builder, (node_nsdecl*)Node);
        case NodeType_If:           return Append(Builder, (node_if*)Node);
        case NodeType_Else:         return Append(Builder, (node_else*)Node);
        case NodeType_While:        return Append(Builder, (node_while*)Node);
        case NodeType_Address:      return Append(Builder, (node_address*)Node);
        case NodeType_Deref:        return Append(Builder, (node_deref*)Node);
        case NodeType_Index:        return Append(Builder, (node_index*)Node);
        case NodeType_Negate:       return Append(Builder, (node_negate*)Node);
        case NodeType_Not:          return Append(Builder, (node_not*)Node);
        case NodeType_As:           return Append(Builder, (node_as*)Node);
        case NodeType_Break:        return Append(Builder, (node_break*)Node);
        case NodeType_Continue:     return Append(Builder, (node_continue*)Node);
        case NodeType_BraceInit: Builder->Append("{}"); return TRANSLATE_OK;
        default:
            assert(false);
    }
    return TRANSLATE_OK;
}
extern "C" int Translate(compiler & Compiler) {
    GlobalCompiler = ::Compiler = &Compiler;

    InternalFunctions["console.write(string)"] = R"(
uint32 write(string str) {
    DWORD charsWritten;
    ::WriteConsoleA(::console::outHandle, str.begin, str.count(), &charsWritten, 0);
    return charsWritten;
}
)";
    InternalFunctions["console.read(char*,uint64)"] = R"(
uint64 read(char* buf, uint64 max) {
    DWORD charsRead = 0;
    ::ReadConsoleA(::console::inHandle, buf, max, &charsRead, 0);
    return charsRead;
}
)";
    InternalFunctions["os.allocate(uint64)"] = R"(
void* allocate(uint64 size) {
    return ::malloc(size);
}
)";
    InternalFunctions["os.free(void*)"] = R"(
void free(void* ptr) {
    return ::free(ptr);
}
)";

    char CppFileName[256];
    char BatFileName[256];
    sprintf(CppFileName, "%s%s.temp.cpp", Compiler.TempDir, ToString(Compiler.Parser->OutFile).data());
    sprintf(BatFileName, "%slng_cl_build.bat", Compiler.TempDir);
    FILE* CppFile = fopen(CppFileName, "wb");
    if (CppFile) {
        auto& Parser = *Compiler.Parser;
        fputs(CppFile, R"(
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <windows.h>
using int8    = int8_t;
using int16   = int16_t;
using int32   = int32_t;
using int64   = int64_t;
using uint8   = uint8_t;
using uint16  = uint16_t;
using uint32  = uint32_t;
using uint64  = uint64_t;
using float32 = float;
using float64 = double;
template<class type, size_t size>
struct array {
    type values[size];
    type& operator[](size_t i) { return values[i]; }
};
struct string {
    char* begin;
    char* end;
    string(char* begin, char* end) : begin(begin), end(end) {}
    string(char* strz) : string(strz, strz + strlen(strz)) {}
    string(char* begin, size_t count) : begin(begin), end(begin + count) {}
    string() : begin(0), end(0) {}
    uint32 count() { return end - begin; }
    char at(uint32 i) { return begin[i]; }
};
string operator ""_str(const char* Str, size_t Count) {
    return string((char*)Str, Count);
}
namespace console {
    HANDLE outHandle = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE inHandle = GetStdHandle(STD_INPUT_HANDLE);
}
)");
        string_builder Definitions;
        string_builder CppFileBuilder;
        for (auto& Pair : Parser.GlobalScope->Types) {
            auto TypeName = Pair.first;
            auto Type = Pair.second;
            if (!Parser.IsBuiltIn(Type)) {
                if (TypeName == Type->Identifier) {
                    if (AppendTypeDefinition(Parser, Definitions, Type) != TRANSLATE_OK)
                        return TRANSLATE_ERROR;
                    bool UseUnions = false;
                    for (auto Member : Type->Members) {
                        if (Member->AstType == NodeType_MemberVarDecl && ((node_membervardecl*)Member)->Offset.Custom)
                            UseUnions = true;
                    }
                    if (UseUnions) {
                        CppFileBuilder.Append("union ");
                        AppendIdentifier(&CppFileBuilder, Type);
                        CppFileBuilder.Append(";\n");
                    }
                    else {
                        CppFileBuilder.Append("struct ");
                        AppendIdentifier(&CppFileBuilder, Type);
                        CppFileBuilder.Append(";\n");
                    }
                }
            }
        }
        CppFileBuilder.Append(Definitions.GetString());
        for (auto Node : Parser.GlobalNode->Declarations) {
            if (Node->AstType != NodeType_TypeDecl) {
                if (Append(0, Node) != TRANSLATE_OK)
                    return TRANSLATE_ERROR;
                if (Node->AstType == NodeType_VarDecl)
                    VarsBuilder.Append(";\n");
            }
        }

        CppFileBuilder.Append("\n/* Variables */\n\n");
        CppFileBuilder.Append(VarsBuilder.GetString());
        CppFileBuilder.Append("\n/* Declaretions */\n\n");
        CppFileBuilder.Append(FuncHeaderBuilder.GetString());
        CppFileBuilder.Append("\n/* Definitions */\n\n");
        CppFileBuilder.Append(FuncBuilder.GetString());
        CppFileBuilder.Append("\n/* Member Definitions */\n\n");
        CppFileBuilder.Append(MemberFunctionDefinitions.GetString());

        auto CppFileStr = CppFileBuilder.GetString();
        fwrite(CppFileStr.data(), CppFileStr.size(), 1, CppFile);
        fclose(CppFile);

        char MSVCDir[512];
        DWORD MSVCDirSize = GetEnvironmentVariableA("VCToolsInstallDir", MSVCDir, sizeof(MSVCDir));
        if (MSVCDirSize == 0) {
            puts("Can't find 'VCToolsInstallDir' environment variable. Make sure you are compiling from Developer Command Prompt for VS");
            return TRANSLATE_ERROR;
        }
        // TODO Support x86
        char MSVCPath[512];
        sprintf(MSVCPath, "%sbin\\Hostx64\\x64\\cl.exe", MSVCDir);

        char MSVCArgs[256];
        sprintf(MSVCArgs, "%s -nologo -Zi -link -out:%s", CppFileName, ToString(Parser.OutFile).data());

        string_builder BatBuilder;
        BatBuilder.Append("@echo off\nif not exist ");
        BatBuilder.Append(Compiler.BinDir);
        BatBuilder.Append(" mkdir ");
        BatBuilder.Append(Compiler.BinDir);
        BatBuilder.Append("\npushd ");
        BatBuilder.Append(Compiler.BinDir);
        BatBuilder.Append("\n\"");
        BatBuilder.Append(MSVCPath);
        BatBuilder.Append("\" ");
        BatBuilder.Append(MSVCArgs);
        BatBuilder.Append("\npopd\nexit /b %errorlevel%");
        FILE* BatFile = fopen(BatFileName, "wb");
        if (BatFile) {
            auto BatStr = BatBuilder.GetString();
            fwrite(BatStr.data(), BatStr.size(), 1, BatFile);
            fclose(BatFile);
            if (system(BatFileName) == 0)
                return TRANSLATE_OK;
            else {
                puts("Failed to compile");
                return TRANSLATE_ERROR;
            }
            return TRANSLATE_OK;
        }
        else {
            printf("Can't open %s", BatFileName);
            return TRANSLATE_ERROR;
        }
    }
    else {
        printf("Can't open %s", CppFileName);
        return TRANSLATE_ERROR;
    }
}