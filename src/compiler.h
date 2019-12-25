#pragma once
#include "common.h"
#define COMPILE_SUCCESS 0
#define COMPILE_ERROR 1
struct lexer;
struct parser;
struct compiler {
    lexer* Lexer = 0;
    parser* Parser = 0;

    string_view Source;

    directory_view SourceFileName;
    directory_view CurrentDirectory;
    directory_view CompilerDirectory;

    char TempDir[512];
    char BinDir[512];
    int ExitCode;
    void Compile();
    void Free();
    // NOTE: Add messages only using 'ReportMessage'!
    list<message> Messages;

    list<void*> FilesContents;
    string_view LoadFileContents(FILE* File) {
        fseek(File, 0, SEEK_END);
        auto FileSize = ftell(File);
        fseek(File, 0, SEEK_SET);
        auto FileContents = new char[FileSize];
        FilesContents.push_back(FileContents);
        fread(FileContents, FileSize, 1, File);
        return {FileContents, (size_t)FileSize};
    }

    list<directory_stdstring> CompilingFilePaths;

    void PrintMessages();

    void ReportMessage(const message& Msg) {
        _ReportMessage(this, Msg);
    }
    void (*_ReportMessage)(compiler*, const message& Msg) = 0;
    static compiler* Instance;
};
compiler* compiler::Instance = 0;
#define COMPILER (*compiler::Instance)
void ReportMessage(const message& Message) {
    compiler::Instance->ReportMessage(Message);
}
inline void ReportMessage() {
    ReportMessage({});
}
template<class ...types>
inline void ReportMessage(types... Types) {
    message Message;
    Append(Message, Types...);
    ReportMessage(Message);
}
#if 1
template<class ...types>
inline void ReportError_(string_view File, u32 Line, location Location, types... Types) {
    message Message;
    Append(Message, File);
    Append(Message, ':');
    Append(Message, Line);
    Append(Message, ":\n");
    Append(Message, Location);
    Append(Message, ": Error: ");
    Append(Message, Types...);
    Message.Location = Location;
    ReportMessage(Message);
}
#define ReportError(Loc, ...) ReportError_(__FILE__, __LINE__, Loc, __VA_ARGS__)
#else
template<class ...types>
inline void ReportError(location Location, types... Types) {
    message Message;
    Append(Message.String, CompiledFile);
    Append(Message.String, Location);
    Append(Message.String, ": ");
    Append(Message.String, Types...);
    Message.Location = Location;
    ReportMessage(Message);
}
#endif