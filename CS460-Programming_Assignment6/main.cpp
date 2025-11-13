#include <iostream>
#include <fstream>
#include <regex>
#include <vector>
#include <sstream>
#include <unordered_set>

enum class TokenType {
    // special characters
    L_PAREN, R_PAREN, L_BRACKET, R_BRACKET, L_BRACE, R_BRACE, DOUBLE_QUOTE, SINGLE_QUOTE,
    SEMICOLON, COMMA, ASSIGNMENT_OPERATOR, PLUS, MINUS, ASTERISK, DIVIDE, MODULO, CARET,
    LT, GT, LT_EQUAL, GT_EQUAL, BOOLEAN_AND, BOOLEAN_OR, BOOLEAN_NOT, BOOLEAN_EQUAL,
    BOOLEAN_NOT_EQUAL, ESCAPED_CHARACTER,

    // literals and identifiers
    STRING, INTEGER, CHARACTER, IDENTIFIER,

    //keywords
    BOOLEAN_TRUE, BOOLEAN_FALSE, IF_STATEMENT, ELSE_STATEMENT, FOR_STATEMENT, WHILE_STATEMENT,
    RETURN_STATEMENT, GETCHAR_FUNCTION, PRINTF_STATEMENT, SIZEOF_STATEMENT, MAIN_PROCEDURE,
    PROCEDURE_DECLARATION, FUNCTION_DECLARATION, VOID_STATEMENT, CHAR_STATEMENT, BOOL_STATEMENT,
    INT_STATEMENT,

    // Other tokens
    /*LETTER, DIGIT, HEX_DIGIT, DOUBLE_QUOTED_STRING, SINGLE_QUOTED_STRING,
    LETTER_UNDERSCORE, LETTER_DIGIT_UNDERSCORE, WHOLE_NUMBER, IDENTIFIER_TAIL,
    IDENTIFIER_LIST, IDENTIFIER_ARRAY_LIST, DATATYPE_SPECIFIER, NUMERICAL_OPERAND,
    NUMERICAL_OPERATOR, BOOLEAN_OPERATOR, NUMERICAL_EXPRESSION, BOOLEAN_EXPRESSION,
    INITIALIZATION_EXPRESSION, EXPRESSION, ITERATION_ASSIGNMENT, SELECTION_STATEMENT,
    ITERATION_STATEMENT, ASSIGNMENT_STATEMENT, USER_DEFINED_FUNCTION, USER_DEFINED_PROCEDURE_CALL_STATEMENT,
    DECLARATION_STATEMENT, STATEMENT, COMPOUND_STATEMENT, BLOCK_STATEMENT,
    PARAMETER_LIST, PROGRAM_TAIL, PROGRAM,*/

    NEWLINE,
    END
};

void printError(const std::string& tokenType, const int& lineNumber) {
    std::cout << "Syntax error on line " << lineNumber << ": invalid " << tokenType << std::endl;
}

struct Token {
    TokenType type;
    std::string token;
    int line;
};

int globalScope = 0; // used for keeping track of scope
int scopeCounter = 0;

std::vector<Token> tokenList;

TokenType checkForKeyword(const std::string& token) { // makes IDENTIFIER tokens more specific
    if (token == "if") return TokenType::IF_STATEMENT;
    if (token == "else") return TokenType::ELSE_STATEMENT;
    if (token == "for") return TokenType::FOR_STATEMENT;
    if (token == "while") return TokenType::WHILE_STATEMENT;
    if (token == "printf") return TokenType::PRINTF_STATEMENT;
    if (token == "getchar") return TokenType::GETCHAR_FUNCTION;
    if (token == "sizeof") return TokenType::SIZEOF_STATEMENT;
    if (token == "function") return TokenType::FUNCTION_DECLARATION;
    if (token == "procedure") return TokenType::PROCEDURE_DECLARATION;
    if (token == "main") return TokenType::MAIN_PROCEDURE;
    if (token == "return") return TokenType::RETURN_STATEMENT;
    if (token == "void") return TokenType::VOID_STATEMENT;
    if (token == "char") return TokenType::CHAR_STATEMENT;
    if (token == "bool") return TokenType::BOOL_STATEMENT;
    if (token == "int") return TokenType::INT_STATEMENT;
    if (token == "TRUE") return TokenType::BOOLEAN_TRUE;
    if (token == "FALSE") return TokenType::BOOLEAN_FALSE;
    return TokenType::IDENTIFIER;
}

void storeTokens(const TokenType& tokenType, const std::string& token, const int& lineNum) {
    TokenType updatedTokenType;
    if (tokenType == TokenType::IDENTIFIER) {
        updatedTokenType = checkForKeyword(token);
    } else { updatedTokenType = tokenType; }

    tokenList.push_back({updatedTokenType, token, lineNum});
}

struct CSTNode {
    std::string value;
    CSTNode* left = nullptr;
    CSTNode* right = nullptr;
};

struct TokenStream { // struct to read in tokens
    const std::vector<Token>& stream;
    size_t i = 0;

    const Token& peek(size_t k = 0) const { // peek at current token or future token
        size_t index = std::min(i + k, stream.size() - 1);
        return stream[index];
    };

    const Token& next() { // go to the next token
        if (i >= stream.size()) {
            return stream.back();
        }
        return stream[i++];
    }

    bool isMatching(TokenType type) { // check if the token matches a given token
        if (peek().type == type) {
            next();
            return true;
        }
        return false;
    }

    bool expect(TokenType type, const std::string& token) { // throw an error if the token is not a given token
        if (isMatching(type)) {
            return true;
        }
        std::cerr << "Syntax error on line " << peek().line << ": " << token << " expected." << std::endl;
        return false;
    }

};

CSTNode* createCSTNode(const std::string& value) {
    return new CSTNode{value, nullptr, nullptr};
}

bool isReservedToken(const Token& t) { // check for reserved tokens
    switch (t.type) {
        case TokenType::IF_STATEMENT:
            case TokenType::ELSE_STATEMENT:
        case TokenType::FOR_STATEMENT:
        case TokenType::WHILE_STATEMENT:
        case TokenType::RETURN_STATEMENT:
        case TokenType::PRINTF_STATEMENT:
        case TokenType::GETCHAR_FUNCTION:
        case TokenType::SIZEOF_STATEMENT:
        case TokenType::FUNCTION_DECLARATION:
        case TokenType::PROCEDURE_DECLARATION:
        case TokenType::MAIN_PROCEDURE:
        case TokenType::VOID_STATEMENT:
        case TokenType::CHAR_STATEMENT:
        case TokenType::BOOL_STATEMENT:
        case TokenType::INT_STATEMENT:
        case TokenType::BOOLEAN_TRUE:
        case TokenType::BOOLEAN_FALSE:
            return true;
        default: return false;
    }
}

std::pair<bool,std::string> removeComments(std::fstream& file) {
    std::ostringstream noComments;
        enum State {Default, Slash, LineComment, BlockComment, Star, Quote, PossibleBlockError};

        State state = Default;
        char c;
        int currentLine = 1, blockCommentLine = 0; // keeps track of line number to tell the user where errors are

        while ((c = file.get()) != EOF) {
            switch (state) {
                case Default:
                    if (c == '/') {
                        state = Slash;
                    } else if (c == '"') {
                        state = Quote;
                        noComments << c;
                    } else if (c == '*') {
                        state = PossibleBlockError;
                        noComments << c;
                    } else if (c == '\n') {
                        currentLine++;
                        noComments << c;
                    } else {
                        noComments << c;
                    }
                break;
                case Slash:
                    if (c == '/') {
                        state = LineComment;
                        noComments << "  ";
                    } else if (c == '*') {
                        state = BlockComment;
                        noComments << "  ";
                        blockCommentLine = currentLine;
                    } else if (c == '\n') {
                        state = Default;
                        currentLine++;
                        noComments << "/";
                        noComments << c;
                    } else {
                        state = Default;
                        noComments << "/";
                        noComments << c;
                    }
                break;
                case LineComment:
                    if (c == '\n') {
                        noComments << "\n";
                        state = Default;
                        currentLine++;
                    } else {
                        noComments << " ";
                    }
                break;
                case BlockComment:
                    if (c == '*') {
                        state = Star;
                        noComments << " ";
                    } else if (c == '\n') {
                        noComments << "\n";
                        currentLine++;
                    } else {
                        noComments << " ";
                    }
                break;
                case Star:
                    if (c == '/') {
                        state = Default;
                        noComments << " ";
                    } else if (c == '*') {
                        noComments << " ";
                    } else if (c == '\n') {
                        state = BlockComment;
                        noComments << "\n";
                        currentLine++;
                    } else {
                        state = BlockComment;
                        noComments << " ";
                    }
                break;
                case Quote:
                    if (c == '"') {
                        state = Default;
                        noComments << c;
                    } else if (c == '\n') {
                        noComments << c;
                        currentLine++;
                    } else {
                        noComments << c;
                    }
                break;
                case PossibleBlockError:
                    if (c == '/') {
                        std::cout << "\nERROR: Program contains C-style, unterminated comment on line " << currentLine << std::endl;
                        return {false, ""};
                    }
                    if (c == '\n') {
                        state = Default;
                        noComments << c;
                        currentLine++;
                    } else {
                        state = Default;
                        noComments << c;
                    }
                break;
            }
        }
        if (state == BlockComment) {
            std::cout << "\nERROR: Program contains C-style, unterminated comment on line " << blockCommentLine << std::endl;
            return {false, ""};
        }
    return {true, noComments.str()};
}

bool tokenize(std::istringstream& file) {
    enum State {Default, DoubleQuote, SingleQuote};
    State state = Default;
    int c;
    int currentLine = 1;
    int quoteLine; // keeps track of the original line number for quotes for unterminated quotes
    std::string tokenString; // holds string for tokenization
    while ((c = file.get()) != EOF) {
        switch (state) {
            case Default:
                if (c == '\n') currentLine++;
                else if (c == '(') storeTokens(TokenType::L_PAREN, "(", currentLine);
                else if (c == ')') storeTokens(TokenType::R_PAREN, ")", currentLine);
                else if (c == '[') storeTokens(TokenType::L_BRACKET, "[", currentLine);
                else if (c == ']') storeTokens(TokenType::R_BRACKET, "]", currentLine);
                else if (c == '{') storeTokens(TokenType::L_BRACE, "{", currentLine);
                else if (c == '}') storeTokens(TokenType::R_BRACE, "}", currentLine);
                else if (c == ';') storeTokens(TokenType::SEMICOLON, ";", currentLine);
                else if (c == ',') storeTokens(TokenType::COMMA, ",", currentLine);
                else if (c == '+') storeTokens(TokenType::PLUS, "+", currentLine);
                else if (c == '*') storeTokens(TokenType::ASTERISK, "*", currentLine);
                else if (c == '/') storeTokens(TokenType::DIVIDE, "/", currentLine);
                else if (c == '%') storeTokens(TokenType::MODULO, "%", currentLine);
                else if (c == '^') storeTokens(TokenType::CARET, "^", currentLine);
                else if (c == '<') {
                    if (file.peek() == '=') {
                        file.get();
                        storeTokens(TokenType::LT_EQUAL, "<=", currentLine);
                    }
                    else storeTokens(TokenType::LT, "<", currentLine);
                } else if (c == '>') {
                    if (file.peek() == '=') {
                        file.get();
                        storeTokens(TokenType::GT_EQUAL, ">=", currentLine);
                    }
                    else storeTokens(TokenType::GT, ">", currentLine);
                } else if (c == '&') {
                    if (file.peek() == '&') {
                        file.get();
                        storeTokens(TokenType::BOOLEAN_AND, "&&", currentLine);
                    }
                    else storeTokens(TokenType::CHARACTER, "&", currentLine);
                } else if (c == '|') {
                    if (file.peek() == '|') {
                        file.get();
                        storeTokens(TokenType::BOOLEAN_OR, "||", currentLine);
                    }
                    else storeTokens(TokenType::CHARACTER, "|", currentLine);
                } else if (c == '!') {
                    if (file.peek() == '=') {
                        file.get();
                        storeTokens(TokenType::BOOLEAN_NOT_EQUAL, "!=", currentLine);
                    }
                    else storeTokens(TokenType::BOOLEAN_NOT, "!", currentLine);
                } else if (c == '=') {
                    if (file.peek() == '=') {
                        file.get();
                        storeTokens(TokenType::BOOLEAN_EQUAL, "==", currentLine);
                    }
                    else storeTokens(TokenType::ASSIGNMENT_OPERATOR, "=", currentLine);
                } else if (c == '"') {
                    storeTokens(TokenType::DOUBLE_QUOTE, "\"", currentLine);
                    quoteLine = currentLine;
                    state = DoubleQuote;
                } else if (c == '\'') {
                    storeTokens(TokenType::SINGLE_QUOTE, "\'", currentLine);
                    quoteLine = currentLine;
                    state = SingleQuote;
                } else if (c == '-') {
                    if (isdigit(file.peek())) {
                        tokenString = "-";
                        while (isdigit(file.peek())) {
                            tokenString += static_cast<char>(file.get());
                        }
                        storeTokens(TokenType::INTEGER, tokenString, currentLine);
                        tokenString = "";
                    }
                    else storeTokens(TokenType::MINUS, "-", currentLine);
                } else if (isalpha(c)) {
                    tokenString = c;
                    if (isalpha(file.peek()) || isdigit(file.peek()) || file.peek() == '_') {
                        while (isalpha(file.peek()) || isdigit(file.peek()) || file.peek() == '_') {
                            tokenString += static_cast<char>(file.get());
                        }
                        storeTokens(TokenType::IDENTIFIER, tokenString, currentLine);
                        tokenString = "";
                    }
                    else {
                        storeTokens(TokenType::IDENTIFIER, tokenString, currentLine);
                        tokenString = "";
                    }
                } else if (isdigit(c)) {
                    if (isdigit(file.peek())) {
                        tokenString = c;
                        while (isdigit(file.peek())) {
                            tokenString += static_cast<char>(file.get());
                        }
                        storeTokens(TokenType::INTEGER, tokenString, currentLine);
                        tokenString = "";
                    } else if (isalpha(file.peek())) {
                        printError("INTEGER", currentLine); // throws an error if an int is followed by a letter
                        return false;
                    }
                    else {
                        tokenString = c;
                        storeTokens(TokenType::INTEGER, tokenString, currentLine);
                        tokenString = "";
                    }
                }
            break;
            case DoubleQuote:
                if (c == '\"') {
                    // Store the string and closing quote when finished
                    storeTokens(TokenType::STRING, tokenString, currentLine);
                    storeTokens(TokenType::DOUBLE_QUOTE, "\"", currentLine);
                    tokenString = "";
                    state = Default;
                } else if (c == '\\') {
                    // Keep escape characters in the string
                    int p = file.get();
                    if (p == EOF) { // throw error if the quote is never closed
                        std::cerr << "Syntax error on line " << quoteLine
                                  << ": unterminated string quote.\n";
                        return false;
                    }
                    if (p == 'x') {
                        // check that hex digits are formatted correctly
                        tokenString.push_back('\\');
                        tokenString.push_back('x');
                        int h1 = file.peek();
                        if (isxdigit(h1)) {
                            tokenString.push_back(static_cast<char>(file.get()));
                            int h2 = file.peek();
                            if (isxdigit(h2)) tokenString.push_back(static_cast<char>(file.get()));
                        }
                    } else {
                        tokenString.push_back('\\');
                        tokenString.push_back(static_cast<char>(p));
                    }
                } else {
                    if (c == '\n') currentLine++;
                    tokenString.push_back(static_cast<char>(c));
                }
            break;
            case SingleQuote: {
                // replace character that was read in to use later
                file.putback(static_cast<char>(c));
                std::string quoteContents;
                int unitCount = 0;               // count of logical units inside the quotes
                bool sawEscapeUnitOnly = false;  // check if the quotes contain only an escape character

                while (true) {
                    int ch = file.get();
                    if (ch == EOF) { // check for unterminated quotes
                        std::cerr << "Syntax error on line " << quoteLine
                                  << ": unterminated string quote.\n";
                        return false;
                    }
                    if (ch == '\n') currentLine++;
                    if (ch == '\'') {
                        if (!quoteContents.empty()) { // store as an escape character or a string depending on the contents
                            if (unitCount == 1 && sawEscapeUnitOnly) {
                                storeTokens(TokenType::ESCAPED_CHARACTER, quoteContents, currentLine);
                            } else {
                                storeTokens(TokenType::STRING, quoteContents, currentLine);
                            }
                        }
                        storeTokens(TokenType::SINGLE_QUOTE, "\'", currentLine);
                        state = Default;
                        break;
                    }

                    if (ch == '\\') { // check for escape characters
                        std::string escape = "\\";
                        int p = file.get();
                        if (p == EOF) {
                            std::cerr << "Syntax error on line " << currentLine
                                      << ": unterminated escape in character/string.\n";
                            return false;
                        }
                        escape.push_back(static_cast<char>(p));
                        if (p == 'x') {
                            int h1 = file.peek();
                            if (isxdigit(h1)) {
                                escape.push_back(static_cast<char>(file.get()));
                                int h2 = file.peek();
                                if (isxdigit(h2)) escape.push_back(static_cast<char>(file.get()));
                            }
                        }
                        quoteContents += escape;
                        unitCount += 1;
                        sawEscapeUnitOnly = (unitCount == 1);  // only true if we have only seen the escape character
                    } else {
                        quoteContents.push_back(static_cast<char>(ch));
                        unitCount += 1;
                        sawEscapeUnitOnly = false;             // no longer on an escape character
                    }
                }
                break;
            }
        }
    }
    if (state == DoubleQuote) {
        std::cerr << "Syntax error on line " << quoteLine << ": unterminated string quote.\n";
        return false;
    }
    if (state == SingleQuote) {
        std::cerr << "Syntax error on line " << quoteLine << ": unterminated string quote.\n";
        return false;
    }
    tokenList.push_back({TokenType::END, "<END>", currentLine});
    return true;
}

CSTNode* buildCSTFromTokens(const std::vector<Token>& tokens) {
    CSTNode* root = nullptr;
    CSTNode* prevLineLast = nullptr;   // last token node of previous non-empty line
    CSTNode* currentLineFirst  = nullptr;  // first token of current line
    CSTNode* currentLineLast   = nullptr;  // last token of current line
    int lineNumber = tokens[0].line;

    for (const auto& t : tokens) {
        if (t.type == TokenType::END) break;
        if (t.line > lineNumber) { // Set the first token of the next line as the child (left pointer) of the last token on the current line
            lineNumber = t.line;
            if (currentLineFirst) {
                if (prevLineLast && !prevLineLast->left) {
                    prevLineLast->left = currentLineFirst;
                }
                prevLineLast = currentLineLast;
                currentLineFirst = currentLineLast = nullptr;
            }
        }

        CSTNode* node = createCSTNode(t.token);
        if (!root) root = node; // first token becomes root
        if (!currentLineFirst) {
            currentLineFirst = currentLineLast = node; // first token in this line
        } else {
            currentLineLast->right = node;         // right sibling
            currentLineLast = node;
        }
    }
    // handle a file that doesn't end with a new line
    if (currentLineFirst) {
        if (prevLineLast && !prevLineLast->left) {
            prevLineLast->left = currentLineFirst;
        }
    }
    return root;
}

void printCST (CSTNode* root) {
    if (!root) return;
    std::vector<CSTNode*> level{root};

    while (!level.empty()) { // print CST level by level
        std::vector<CSTNode*> next;
        bool firstOnLine = true;
        for (CSTNode* head : level) {
            for (CSTNode* n = head; n; n = n->right) {
                if (!firstOnLine) std::cout << "   ";
                std::cout << n->value;
                firstOnLine = false;
                if (n->left) next.push_back(n->left); // add child if it exists
            }
        }
        std::cout << "\n";
        level.swap(next); // go to next line
    }
}

enum class DataTypes { Char, Bool, Int, None };

struct SymbolTableNode {
    std::string identifierName;
    std::string identifierType;
    DataTypes dataType;
    bool dataTypeIsArray = false;
    int dataTypeArraySize{};
    int scope{};
    SymbolTableNode* parameterList = nullptr;
    SymbolTableNode* next = nullptr;

    // constructors
    SymbolTableNode() = default;
    SymbolTableNode(const std::string & id_name, const std::string & id_type, DataTypes newDataType, bool is_array, int data_type_array_size, int newScope) {
        identifierName = id_name;
        identifierType = id_type;
        dataType = newDataType;
        dataTypeIsArray = is_array;
        dataTypeArraySize = data_type_array_size;
        scope = newScope;
    }
};

SymbolTableNode* head;
SymbolTableNode* tail;

// adds a normal declarator to the symbol table
void addSymbolTableNode(const std::string& idName, const std::string& idType, DataTypes dataType, int dataTypeArraySize, int scope) {
    bool isArray = false;
    if (dataTypeArraySize != 0) isArray = true;
    auto* nodeToBeAdded = new SymbolTableNode{idName, idType, dataType, isArray, dataTypeArraySize, scope};
    if (!head) {
        head = nodeToBeAdded;
        tail = nodeToBeAdded;
    } else {
        tail->next = nodeToBeAdded;
        tail = nodeToBeAdded;
    }
}

// adds a declarator from a parameter list
void addParameterList(const std::string& idName, const std::string& idType, DataTypes dataType, int dataTypeArraySize, int scope) {
    bool isArray = false;
    if (dataTypeArraySize != 0) isArray = true;
    auto* nodeToBeAdded = new SymbolTableNode{idName, idType, dataType, isArray, dataTypeArraySize, scope};
    if (!head) {
        std::cerr << "Adding parameter list to empty syntax tree.\n";
        return;
    }
    if (tail->parameterList == nullptr) tail->parameterList = nodeToBeAdded; // adds to parameter list of the most recent node
    else {
        auto* currentNode = tail->parameterList;
        while (currentNode->next) {
            currentNode = currentNode->next;
        }
        currentNode->next = nodeToBeAdded;
    }
}

static const char* dataTypeToString(DataTypes dt) { // for printing datatypes
    switch (dt) {
        case DataTypes::Char: return "char";
        case DataTypes::Int:  return "int";
        case DataTypes::Bool: return "bool";
        default:              return "NOT APPLICABLE";
    }
}

static const char* yesNo(bool b) { return b ? "yes" : "no"; } // for printing if a declarator is an array or not

static void printMainNode(const SymbolTableNode* n) { // for printing normal nodes
    printf("%22s %s\n", "IDENTIFIER_NAME:",       n->identifierName.c_str());
    printf("%22s %s\n", "IDENTIFIER_TYPE:",       n->identifierType.c_str());
    printf("%22s %s\n", "DATATYPE:",              dataTypeToString(n->dataType));
    printf("%22s %s\n", "DATATYPE_IS_ARRAY:",     yesNo(n->dataTypeIsArray));
    printf("%22s %d\n", "DATATYPE_ARRAY_SIZE:",   n->dataTypeArraySize);
    printf("%22s %d\n", "SCOPE:",                 n->scope);
    printf("\n");
}

static void printParamNode(const SymbolTableNode* n) { // for printing parameter list nodes
    printf("%22s %s\n", "IDENTIFIER_NAME:",       n->identifierName.c_str());
    printf("%22s %s\n", "DATATYPE:",              dataTypeToString(n->dataType));
    printf("%22s %s\n", "DATATYPE_IS_ARRAY:",     yesNo(n->dataTypeIsArray));
    printf("%22s %d\n", "DATATYPE_ARRAY_SIZE:",   n->dataTypeArraySize);
    printf("%22s %d\n", "SCOPE:",                 n->scope);
    printf("\n");
}

void printSymbolTable(const SymbolTableNode* head) {
    if (!head) return;
    for (auto* cur = head; cur; cur = cur->next) { // print all normal nodes
        printMainNode(cur);
    }
    for (auto* cur = head; cur; cur = cur->next) { // print all parameter list nodes
        if (cur->parameterList) {
            printf("%22s %s\n", "PARAMETER LIST FOR:", cur->identifierName.c_str());
            for (auto* p = cur->parameterList; p; p = p->next) {
                printParamNode(p);
            }
        }
    }
}

bool parameterAlreadyExists(const std::string& name, int lineNumber) { // check if a parameter is already in a parameter list
    if (!tail || !tail->parameterList) return false;
    for (auto* p = tail->parameterList; p; p = p->next) {
        if (p->identifierName == name) {
            std::cerr << "Error on line " << lineNumber
                      << ": parameter \"" << name
                      << "\" is already defined in this parameter list\n";
            return true;
        }
    }
    return false;
}

bool isAlreadyDeclared(const std::string& name, int scope, int lineNumber) { // check if a declarator is already declared in a relevant scope
    SymbolTableNode* currentParameter = nullptr;
    for (auto* cur = head; cur; cur = cur->next) {
        if (cur->identifierName == name) {
            if (cur->scope == 0) {
                std::cerr << "Error on line " << lineNumber
                          << ": variable \"" << name
                          << "\" is already defined globally\n";
                return true;
            }
            if (cur->scope == scope) {
                std::cerr << "Error on line " << lineNumber
                          << ": variable \"" << name
                          << "\" is already defined locally\n";
                return true;
            }
        }
        if (cur->parameterList) currentParameter = cur->parameterList;
    }
    while (currentParameter) { // check for repeat declarators in parameter lists
        if (currentParameter->identifierName == name && currentParameter->scope == scope) {
            std::cerr << "Error on line " << lineNumber
                          << ": variable \"" << name
                          << "\" is already defined locally\n";
            return true;
        }
        currentParameter = currentParameter->next;
    }
    return false;
}

enum class ASTElements {
    DECLARATION, BEGINBLOCK, ASSIGNMENT, ENDBLOCK, RETURN, PRINTF, IF, ELSE, FOR, WHILE, CALL, NONE
};

std::vector<ASTElements> basicAST;

// ---------- Postfix helpers ----------
// check for operators
static bool isOperator(const std::string& t) {
    static const std::unordered_set<std::string> ops = {
        "=", "||", "&&", "==", "!=", "<=", ">=", "<", ">", "+", "-", "*", "/", "%", "!"
    };
    return ops.count(t) > 0;
}

// check for left associative character
static bool isLeftAssociative(const std::string& op) {
    if (op == "=") return false;
    if (op == "!") return false;
    return true;
}

// check operator precedence (higher number is higher precedence)
int precedence(std::string s) {
    if (s == "!")  return 7;
    if (s == "*" || s == "/" || s == "%") return 6;
    if (s == "+" || s == "-")           return 5;
    if (s == "<" || s == ">" || s == "<=" || s == ">=") return 4;
    if (s == "==" || s == "!=")         return 3;
    if (s == "&&")                      return 2;
    if (s == "||")                      return 1;
    if (s == "=")                       return 0;
    return -1; // non-operator
}

// checks for categorizing character
static bool isParen(const std::string& t)   { return t == "(" || t == ")"; }
static bool isBracket(const std::string& t) { return t == "[" || t == "]"; }
// check for identifiers so that parentheses are not removed
static bool looksIdentifier(const std::string& t) {
    if (t.empty()) return false;
    if (!(std::isalpha((unsigned char)t[0]) || t[0] == '_')) return false;
    for (size_t i = 1; i < t.size(); ++i) {
        if (!(std::isalnum((unsigned char)t[i]) || t[i] == '_')) return false;
    }
    return true;
}

// check for function calls for not removing parentheses
static bool isPotentialFunctionCall(const std::vector<std::string>& v, size_t i) {
    return i + 1 < v.size() && looksIdentifier(v[i]) && v[i + 1] == "(";
}

// copies balanced parentheses
static void copyBalanced(const std::vector<std::string>& in, size_t& i, std::vector<std::string>& out, const std::string& openTok, const std::string& closeTok) {
    int depth = 0;
    do {
        if (in[i] == openTok) ++depth;
        else if (in[i] == closeTok) --depth;
        out.push_back(in[i]);
        ++i;
    } while (i < in.size() && depth > 0);
}

// infix to postfix converter
std::vector<std::string> toPostFix(const std::vector<std::string>& infix) {
    std::vector<std::string> output;
    std::vector<std::string> opstack;

    // scan input tokens
    for (size_t i = 0; i < infix.size();) {
        const std::string& tok = infix[i];

        // check for function calls
        if (isPotentialFunctionCall(infix, i)) {
            // push identifier
            output.push_back(infix[i++]);          // function name
            // copy balanced parenthesis with everything inside untouched
            if (i < infix.size() && infix[i] == "(") {
                copyBalanced(infix, i, output, "(", ")");
            }
            continue;
        }

        // check for bracket indexing
        if (tok == "[" ) {
            copyBalanced(infix, i, output, "[", "]");
            continue;
        }

        // check for parentheses used in expressions
        if (tok == "(") {
            opstack.push_back(tok);
            ++i;
            continue;
        }
        // check for closing parentheses
        if (tok == ")") {
            while (!opstack.empty() && opstack.back() != "(") {
                output.push_back(opstack.back());
                opstack.pop_back();
            }
            if (!opstack.empty() && opstack.back() == "(") opstack.pop_back(); // discard "("
            ++i;
            continue;
        }

        // check for operators
        if (isOperator(tok)) {
            // Pop higher-precedence (or equal + left-assoc) operators
            while (!opstack.empty() && isOperator(opstack.back())) {
                const std::string& top = opstack.back();
                int pTop = precedence(top), pTok = precedence(tok);
                if ( (pTop > pTok) || (pTop == pTok && isLeftAssociative(tok)) ) {
                    output.push_back(top);
                    opstack.pop_back();
                } else break;
            }
            opstack.push_back(tok);
            ++i;
            continue;
        }

        // push back plain operands
        output.push_back(tok);
        ++i;
    }

    // push final operators
    while (!opstack.empty()) {
        if (!isParen(opstack.back())) {
            output.push_back(opstack.back());
        }
        opstack.pop_back();
    }
    return output;
}

CSTNode* buildAST(const std::vector<ASTElements>& elements, const std::vector<Token>& tokens) {
    CSTNode* root = nullptr;
    CSTNode* attachPoint   = nullptr;
    int lineNumber = tokens[0].line;
    int currentTokenIndex = 0;

    for (const auto& e : elements) {
        std::vector<std::string> stringsToBeAdded;
        std::vector<Token> tokensToPostFix;
        std::string ASTElementString;
        bool multipleDeclarations = false;
        bool isForLoop = false;
        int iter = 0;
        if (e == ASTElements::DECLARATION) {
            ASTElementString = "DECLARATION";
            currentTokenIndex++;
            if (iter == 0) currentTokenIndex++;
            if (tokens[currentTokenIndex].type == TokenType::COMMA) {
                iter++;
                multipleDeclarations = true;
            }
        }
        else if (e == ASTElements::BEGINBLOCK) ASTElementString = "BEGIN BLOCK";
        else if (e == ASTElements::ENDBLOCK) ASTElementString = "END BLOCK";
        else if (e == ASTElements::ELSE) ASTElementString = "ELSE";
        else if (e == ASTElements::ASSIGNMENT) {
            ASTElementString = "ASSIGNMENT ";
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type == TokenType::IDENTIFIER) {
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                    currentTokenIndex++;
                    break;
                }
                currentTokenIndex++;
            }
            while (tokens[currentTokenIndex].line == lineNumber && tokens[currentTokenIndex].type != TokenType::SEMICOLON) {
                stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                currentTokenIndex++;
            }
        } else if (e == ASTElements::RETURN) {
            ASTElementString = "RETURN ";
            currentTokenIndex++;
            while (tokens[currentTokenIndex].type == TokenType::L_PAREN) {
                currentTokenIndex++;
            }
            while (tokens[currentTokenIndex].line == lineNumber && tokens[currentTokenIndex].type != TokenType::SEMICOLON && tokens[currentTokenIndex].type != TokenType::R_PAREN) {
                stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                currentTokenIndex++;
            }
        } else if (e == ASTElements::PRINTF) {
            ASTElementString = "PRINTF ";
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type == TokenType::DOUBLE_QUOTE || tokens[currentTokenIndex].type == TokenType::SINGLE_QUOTE) {
                    currentTokenIndex++;
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                    break;
                }
                currentTokenIndex++;
            }
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type == TokenType::COMMA) {
                    currentTokenIndex++;
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                }
                currentTokenIndex++;
            }
        } else if (e == ASTElements::IF) {
            int numParens = 0;
            bool isId = false;
            ASTElementString = "IF ";
            currentTokenIndex++;
            /*while (tokens[currentTokenIndex].type == TokenType::L_PAREN) {
                currentTokenIndex++;
            }*/
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type == TokenType::L_PAREN)  numParens++;
                if (tokens[currentTokenIndex].type != TokenType::R_PAREN || ( tokens[currentTokenIndex].type == TokenType::R_PAREN && numParens > 0)) {
                    if (tokens[currentTokenIndex].type == TokenType::R_PAREN) numParens--;
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                }
                currentTokenIndex++;
            }
        } else if (e == ASTElements::FOR) {
            int numParens = 0;
            isForLoop = true;
            ASTElementString = "FOR EXPRESSION 1";
            currentTokenIndex = currentTokenIndex + 2;
            while (tokens[currentTokenIndex].line == lineNumber && tokens[currentTokenIndex].type != TokenType::SEMICOLON) {
                stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                currentTokenIndex++;
            }
            CSTNode* node = createCSTNode(ASTElementString);
            if (!root) root = node;
            if (attachPoint) {
                attachPoint->left = node;
            }

            CSTNode* tail = node;
            stringsToBeAdded = toPostFix(stringsToBeAdded);
            for (const auto& s : stringsToBeAdded) {
                CSTNode* sn = createCSTNode(s);
                tail->right = sn;
                tail = sn;
            }
            attachPoint = tail;
            while (!stringsToBeAdded.empty()) {
                stringsToBeAdded.pop_back();
            }
            currentTokenIndex++;
            ASTElementString = "FOR EXPRESSION 2";
            while (tokens[currentTokenIndex].line == lineNumber && tokens[currentTokenIndex].type != TokenType::SEMICOLON) {
                stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                currentTokenIndex++;
            }
            node = createCSTNode(ASTElementString);
            if (attachPoint) {
                attachPoint->left = node;
            }

            tail = node;
            stringsToBeAdded = toPostFix(stringsToBeAdded);
            for (const auto& s : stringsToBeAdded) {
                CSTNode* sn = createCSTNode(s);
                tail->right = sn;
                tail = sn;
            }
            attachPoint = tail;
            while (!stringsToBeAdded.empty()) {
                stringsToBeAdded.pop_back();
            }
            currentTokenIndex++;
            ASTElementString = "FOR EXPRESSION 3";
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type == TokenType::L_PAREN)  numParens++;
                if (tokens[currentTokenIndex].type != TokenType::R_PAREN || ( tokens[currentTokenIndex].type == TokenType::R_PAREN && numParens > 0)) {
                    if (tokens[currentTokenIndex].type == TokenType::R_PAREN) numParens--;
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                }
                currentTokenIndex++;
            }
            node = createCSTNode(ASTElementString);
            if (!root) root = node;
            if (attachPoint) {
                attachPoint->left = node;
            }

            tail = node;
            stringsToBeAdded = toPostFix(stringsToBeAdded);
            for (const auto& s : stringsToBeAdded) {
                CSTNode* sn = createCSTNode(s);
                tail->right = sn;
                tail = sn;
            }
            attachPoint = tail;
        } else if (e == ASTElements::WHILE) {
            ASTElementString = "WHILE ";
            currentTokenIndex++;
            while (tokens[currentTokenIndex].line == lineNumber) {
                if (tokens[currentTokenIndex].type != TokenType::L_PAREN && tokens[currentTokenIndex].type != TokenType::R_PAREN) {
                    stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                }
                currentTokenIndex++;
            }
        } else if (e == ASTElements::CALL) {
            ASTElementString = "CALL ";
            while (tokens[currentTokenIndex].line == lineNumber && tokens[currentTokenIndex].type != TokenType::SEMICOLON) {
                stringsToBeAdded.push_back(tokens[currentTokenIndex].token);
                currentTokenIndex++;
            }
        }
        if (!isForLoop) {
            CSTNode* node = createCSTNode(ASTElementString);
            if (!root) root = node; // first token becomes root
            // Attach this element as the LEFT child of the previous "tail" (if any)
            if (attachPoint) {
                attachPoint->left = node;
            }

            // Build the right chain of strings for this element
            CSTNode* tail = node;
            stringsToBeAdded = toPostFix(stringsToBeAdded);
            for (const auto& s : stringsToBeAdded) {
                CSTNode* sn = createCSTNode(s);
                tail->right = sn;
                tail = sn;
            }
            attachPoint = tail;
        }

        if (!multipleDeclarations) {
            while (tokens[currentTokenIndex].line == lineNumber) currentTokenIndex++;
        }
        lineNumber = tokens[currentTokenIndex].line;
    }
    return root;
}

/**************** PARSER ****************/

struct ParsedDeclarator { // used to keep track of declarators to be added to the symbol table
    std::string name;
    int arraySize = 0;
};

bool parseDoubleQuotedString(TokenStream& ts) {
    if (!ts.isMatching(TokenType::DOUBLE_QUOTE)) return false; // ensure there is an opening quote
    while (ts.peek().type == TokenType::STRING || ts.peek().type == TokenType::ESCAPED_CHARACTER) { // read in strings and escape characters
        ts.next();
    }
    return ts.expect(TokenType::DOUBLE_QUOTE, "\""); // ensure there is a closing quote
}

bool parseSingleQuotedString(TokenStream& ts) {
    if (!ts.isMatching(TokenType::SINGLE_QUOTE)) return false; // ensure there is an opening quote
    if (ts.peek().type == TokenType::ESCAPED_CHARACTER || ts.peek().type == TokenType::STRING) {
        ts.next();
    }
    return ts.expect(TokenType::SINGLE_QUOTE, "\'"); // ensure there is a closing quote
}

DataTypes parseDataType (TokenStream& ts) { // check for data type
    if (ts.isMatching(TokenType::CHAR_STATEMENT)) return DataTypes::Char;
    if (ts.isMatching(TokenType::BOOL_STATEMENT)) return DataTypes::Bool;
    if (ts.isMatching(TokenType::INT_STATEMENT)) return DataTypes::Int;
    return DataTypes::None;
}

static bool parseOneDeclarator(TokenStream& ts, ParsedDeclarator& out) {
    if (ts.peek().type != TokenType::IDENTIFIER || isReservedToken(ts.peek())) {
        std::cerr << "Syntax error on line " << ts.peek().line << ": reserved word \""
                  << ts.peek().token << "\" cannot be used for the name of a variable." << std::endl;
        return false;
    }
    out.name = ts.peek().token;
    ts.next();

    if (ts.isMatching(TokenType::L_BRACKET)) { // check for an array
        if (ts.peek().type != TokenType::INTEGER ) {
            if (ts.peek().type != TokenType::PLUS ) {
                std::cerr << "Syntax error on line " << ts.peek().line
                          << ": array size expected.\n";
                return false;
            }
            ts.next();
        }
        int size = std::stoi(ts.peek().token);
        if (size < 0) {
            std::cerr << "Syntax error on line "<< ts.peek().line
                      << ": array declaration size must be a positive integer.\n";
            return false;
        }
        out.arraySize = size;
        ts.next(); // size
        if (!ts.expect(TokenType::R_BRACKET, "]")) return false;
    }
    basicAST.push_back(ASTElements::DECLARATION);
    return true;
}

bool parseDeclaration(TokenStream& ts) {
    auto type = parseDataType(ts);
    if (type == DataTypes::None) return false;
    ParsedDeclarator decl{};
    if (!parseOneDeclarator(ts, decl)) return false;
    if (isAlreadyDeclared(decl.name, globalScope, ts.peek().line)) return false; // check and add to symbol table
    addSymbolTableNode(decl.name, "datatype", type, decl.arraySize, globalScope);
    while (ts.isMatching(TokenType::COMMA)) { // check for multiple declarations in one line
        if (!parseOneDeclarator(ts, decl)) return false;
        if (isAlreadyDeclared(decl.name, globalScope, ts.peek().line)) return false;
        addSymbolTableNode(decl.name, "datatype", type, decl.arraySize, globalScope);
    }
    return ts.expect(TokenType::SEMICOLON, ";");
}

bool parsePrintfExpression(TokenStream& ts) {
    if (!ts.isMatching(TokenType::PRINTF_STATEMENT)) return false;
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!parseDoubleQuotedString(ts) && !parseSingleQuotedString(ts)) return false;
    ParsedDeclarator decl{};
    if (ts.isMatching(TokenType::COMMA)) { // check for declarators after commas
        do {
            if (!parseOneDeclarator(ts, decl)) return false;
            basicAST.pop_back();
        } while (ts.isMatching(TokenType::COMMA));
    }
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::PRINTF);
    return ts.expect(TokenType::SEMICOLON, ";");
}

bool parseExpression(TokenStream& ts);

bool parsePrimary(TokenStream& ts) { // check final expressions
    if (ts.peek().type == TokenType::DOUBLE_QUOTE) {
        if (!parseDoubleQuotedString(ts)) return false;
        return true;
    }
    if (ts.isMatching(TokenType::L_PAREN)) {
        if (!parseExpression(ts)) return false;
        return ts.expect(TokenType::R_PAREN, ")");
    }
    if (ts.peek().type == TokenType::SINGLE_QUOTE) {
        return parseSingleQuotedString(ts);
    }
    if (ts.peek().type == TokenType::IDENTIFIER) {
        ts.next();
        if (ts.isMatching(TokenType::L_PAREN)) {
            
            if (!ts.isMatching(TokenType::R_PAREN)) {
                do {
                    if (!parseExpression(ts)) return false;
                    
                } while (ts.isMatching(TokenType::COMMA));
                if (!ts.expect(TokenType::R_PAREN, ")")) return false;
            }
            return true;
        }
        if (ts.isMatching(TokenType::L_BRACKET)) {
            if (!parseExpression(ts)) return false;
            if (!ts.expect(TokenType::R_BRACKET, "]")) return false;
        }
        return true;
    }
    if (ts.peek().type == TokenType::INTEGER) { ts.next(); return true; }
    if (ts.peek().type == TokenType::BOOLEAN_TRUE || ts.peek().type == TokenType::BOOLEAN_FALSE) {
        ts.next(); return true;
    }
    std::cerr << "Syntax error on line " << ts.peek().line << ": expression expected\n";
    return false;
}

bool parseUnary(TokenStream& ts) { // check for minus and not
    if (ts.isMatching(TokenType::MINUS) || ts.isMatching(TokenType::BOOLEAN_NOT)) {
        return parseUnary(ts);
    }
    return parsePrimary(ts);
}

bool parseMulDivMod(TokenStream& ts) {
    if (!parseUnary(ts)) return false;
    while (true) {
        if (ts.isMatching(TokenType::ASTERISK) || ts.isMatching(TokenType::DIVIDE) || ts.isMatching(TokenType::MODULO)) {
            if (!parseUnary(ts)) return false;
        } else break;
    }
    return true;
}

bool parseAddSub(TokenStream& ts) {
    if (!parseMulDivMod(ts)) return false; // check for multiplication, division, and modulo
    while (true) {
        if (ts.isMatching(TokenType::PLUS) || ts.isMatching(TokenType::MINUS)) {
            if (!parseMulDivMod(ts)) return false;
        } else break;
    }
    return true;
}

bool parseRelational(TokenStream& ts) {
    if (!parseAddSub(ts)) return false; // check for addition or subtraction
    while (true) {
        if (ts.isMatching(TokenType::LT) || ts.isMatching(TokenType::GT) ||
            ts.isMatching(TokenType::LT_EQUAL) || ts.isMatching(TokenType::GT_EQUAL) ||
            ts.isMatching(TokenType::BOOLEAN_EQUAL) || ts.isMatching(TokenType::BOOLEAN_NOT_EQUAL)) {
            if (!parseAddSub(ts)) return false;
        } else break;
    }
    return true;
}

bool parseLogicalAnd(TokenStream& ts) {
    if (!parseRelational(ts)) return false; // check for relational
    while (true) {
        if (ts.isMatching(TokenType::BOOLEAN_AND)) {
            if (!parseRelational(ts)) return false;
        } else break;
    }
    return true;
}

bool parseLogicalOr(TokenStream& ts) {
    if (!parseLogicalAnd(ts)) return false; // check for logicalAnd
    while (true) {
        if (ts.isMatching(TokenType::BOOLEAN_OR)) {
            if (!parseLogicalAnd(ts)) return false;
        } else break;
    }
    return true;
}

bool parseExpression(TokenStream& ts) { // parses all expressions
    return parseLogicalOr(ts);
}

bool parseCallStatement(TokenStream& ts) {
    ts.next(); // take identifier
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!ts.isMatching(TokenType::R_PAREN)) {
        do {
            if (!parseExpression(ts)) return false;
        } while (ts.isMatching(TokenType::COMMA));
        if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    }
    basicAST.push_back(ASTElements::CALL);
    return ts.expect(TokenType::SEMICOLON, ";");
}

bool parseAssignmentCore(TokenStream& ts) {
    if (ts.peek().type != TokenType::IDENTIFIER) return false;
    ts.next();
    if (ts.isMatching(TokenType::L_BRACKET)) {
        if (!parseExpression(ts)) return false;
        if (!ts.expect(TokenType::R_BRACKET, "]")) return false;
    }
    if (!ts.expect(TokenType::ASSIGNMENT_OPERATOR, "=")) return false;
    basicAST.push_back(ASTElements::ASSIGNMENT);
    return parseExpression(ts);
}

bool parseAssignment(TokenStream& ts) {
    if (!parseAssignmentCore(ts)) return false;
    return ts.expect(TokenType::SEMICOLON, ";");
}

bool parseReturn(TokenStream& ts) {
    if (!ts.isMatching(TokenType::RETURN_STATEMENT)) return false;
    if (!parseExpression(ts)) return false;
    basicAST.push_back(ASTElements::RETURN);
    return ts.expect(TokenType::SEMICOLON, ";");
}

bool parseIf(TokenStream& ts);
bool parseWhile(TokenStream& ts);
bool parseFor(TokenStream& ts);

bool parseStatement(TokenStream& ts) {
    // declaration statement
    if (ts.peek().type == TokenType::CHAR_STATEMENT ||
        ts.peek().type == TokenType::BOOL_STATEMENT ||
        ts.peek().type == TokenType::INT_STATEMENT) {
        return parseDeclaration(ts);
        }

    // printf statement
    if (ts.peek().type == TokenType::PRINTF_STATEMENT) {
        return parsePrintfExpression(ts);
    }

    // return statement
    if (ts.peek().type == TokenType::RETURN_STATEMENT) {
        return parseReturn(ts);
    }

    // if statement
    if (ts.peek().type == TokenType::IF_STATEMENT) {
        return parseIf(ts);
    }

    // while statement
    if (ts.peek().type == TokenType::WHILE_STATEMENT) {
        return parseWhile(ts);
    }

    // for statement
    if (ts.peek().type == TokenType::FOR_STATEMENT) {
        return parseFor(ts);
    }

    // user call statement
    if (ts.peek().type == TokenType::IDENTIFIER) {
        if (ts.peek(1).type == TokenType::L_PAREN) return parseCallStatement(ts);
        return parseAssignment(ts);
    }

    std::cerr << "Syntax error near line " << ts.peek().line << ": statement expected\n";
    return false;
}

bool parseCompound(TokenStream& ts) {
    if (!parseStatement(ts)) return false;
    while (true) {
        TokenType t = ts.peek().type;
        if (t == TokenType::CHAR_STATEMENT || t == TokenType::BOOL_STATEMENT ||
            t == TokenType::INT_STATEMENT  || t == TokenType::PRINTF_STATEMENT ||
            t == TokenType::RETURN_STATEMENT || t == TokenType::IF_STATEMENT   ||
            t == TokenType::WHILE_STATEMENT  || t == TokenType::FOR_STATEMENT  ||
            t == TokenType::IDENTIFIER) {
                if (!parseStatement(ts)) return false;
            } else break;
    }
    return true;
}

bool parseBlock(TokenStream& ts) {
    if (!ts.expect(TokenType::L_BRACE, "{")) return false;
    basicAST.push_back(ASTElements::BEGINBLOCK);
    if (ts.isMatching(TokenType::R_BRACE)) {
        globalScope--;
        basicAST.push_back(ASTElements::ENDBLOCK);
        return true;
    }
    if (!parseCompound(ts)) return false;
    bool ok = ts.expect(TokenType::R_BRACE, "}");
    basicAST.push_back(ASTElements::ENDBLOCK);
    return ok;
}

bool parseIf(TokenStream& ts) {
    if (!ts.isMatching(TokenType::IF_STATEMENT)) return false;
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!parseExpression(ts)) return false;
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::IF);
    if (!parseBlock(ts)) return false;
    if (ts.isMatching(TokenType::ELSE_STATEMENT)) {
        basicAST.push_back(ASTElements::ELSE);
        if (!parseBlock(ts)) return false;
    }
    return true;
}

bool parseWhile(TokenStream& ts) {
    if (!ts.isMatching(TokenType::WHILE_STATEMENT)) return false;
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!parseExpression(ts)) return false;
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::WHILE);
    return parseBlock(ts);
}

bool parseFor(TokenStream& ts) {
    if (!ts.isMatching(TokenType::FOR_STATEMENT)) return false;
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!ts.isMatching(TokenType::SEMICOLON)) {
        if (!parseAssignmentCore(ts)) {
            std::cerr << "Syntax error near line " << ts.peek().line
                      << ": invalid initialization in for loop\n";
            return false;
        }
        basicAST.pop_back();
        if (!ts.expect(TokenType::SEMICOLON, ";")) return false;
    }

    if (!ts.isMatching(TokenType::SEMICOLON)) {
        if (!parseExpression(ts)) return false;
        if (!ts.expect(TokenType::SEMICOLON, ";")) return false;
    }

    if (!ts.isMatching(TokenType::R_PAREN)) {
        if (!parseAssignmentCore(ts)) {
            std::cerr << "Syntax error near line " << ts.peek().line
                      << ": invalid iteration assignment in for loop\n";
            return false;
        }
        basicAST.pop_back();
        if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    }
    basicAST.push_back(ASTElements::FOR);
    return parseBlock(ts);
}

bool parseMainProcedure(TokenStream& ts) {
    if (!ts.isMatching(TokenType::PROCEDURE_DECLARATION)) return false;
    if (!ts.isMatching(TokenType::MAIN_PROCEDURE))       return false;
    globalScope = ++scopeCounter;
    addSymbolTableNode("main", "procedure", DataTypes::None, 0, globalScope);
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (!ts.isMatching(TokenType::VOID_STATEMENT)) return false;
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::DECLARATION);
    bool ok = parseBlock(ts);
    globalScope = 0;
    return ok;
}

bool parseOneParamDecl(TokenStream& ts) { // check each parameter
    auto dt = parseDataType(ts);
    if (dt == DataTypes::None) return false;
    if (ts.peek().type != TokenType::IDENTIFIER) {
        if (isReservedToken(ts.peek())) {
            std::cerr << "Syntax error on line " << ts.peek().line << ": reserved word \""
                  << ts.peek().token << "\" cannot be used for the name of a variable." << std::endl;
            return false;
        }
        std::cerr << "Syntax error on line " << ts.peek().line << ": parameter name expected\n";
        return false;
    }
    std::string name = ts.peek().token;
    ts.next();

    int arraySize = 0;
    if (ts.isMatching(TokenType::L_BRACKET)) { // check for array declaration
        if (ts.peek().type != TokenType::INTEGER ) {
            if (ts.peek().type != TokenType::PLUS ) {
                std::cerr << "Syntax error on line " << ts.peek().line
                          << ": array size expected.\n";
                return false;
            }
            ts.next();
        }
        int size = std::stoi(ts.peek().token);
        if (size < 0) {
            std::cerr << "Syntax error on line "<< ts.peek().line
                      << ": array declaration size must be a positive integer.\n";
            return false;
        }
        arraySize = size;
        ts.next();
        if (!ts.expect(TokenType::R_BRACKET, "]")) return false;
    }
    if (parameterAlreadyExists(name, ts.peek().line)) return false;
    addParameterList(name, "datatype", dt, arraySize, globalScope);
    return true;
}

bool parseParamDeclList(TokenStream& ts) { // check for one or more parameters
    if (!parseOneParamDecl(ts)) return false;
    while (ts.isMatching(TokenType::COMMA)) {
        if (!parseOneParamDecl(ts)) return false;
    }
    return true;
}

bool parseFunction(TokenStream& ts) {
    if (!ts.isMatching(TokenType::FUNCTION_DECLARATION)) return false;
    // check for data type
    globalScope = ++scopeCounter;
    auto dt = parseDataType(ts);
    if (dt == DataTypes::None) {
        std::cerr << "Syntax error on line " << ts.peek().line << ": function return type expected\n";
        return false;
    }
    if (ts.peek().type != TokenType::IDENTIFIER) { // check for name and ensure it is not reserved
        if (isReservedToken(ts.peek())) {
            std::cerr << "Syntax error on line " << ts.peek().line << ": reserved word \""
                  << ts.peek().token << "\" cannot be used for the name of a function." << std::endl;
            return false;
        }
        std::cerr << "Syntax error on line " << ts.peek().line << ": function name expected\n";
        return false;
    }
    std::string name = ts.peek().token;
    ts.next();

    if (isAlreadyDeclared(name, globalScope, ts.peek().line)) return false; // check and add to symbol table
    addSymbolTableNode(name, "function", dt, 0, globalScope);

    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (ts.isMatching(TokenType::VOID_STATEMENT)) {
    } else {
        if (!parseParamDeclList(ts)) return false;
    }
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::DECLARATION);
    bool ok = parseBlock(ts);
    globalScope = 0;
    return ok;
}

bool parseProcedureDeclaration(TokenStream& ts) {
    if (!ts.isMatching(TokenType::PROCEDURE_DECLARATION)) return false;
    globalScope = ++scopeCounter;
    if (ts.peek().type != TokenType::IDENTIFIER) {
        std::cerr << "Syntax error on line " << ts.peek().line << ": procedure name expected\n";
        return false;
        }
    std::string name = ts.peek().token;
    ts.next();

    if (isAlreadyDeclared(name, globalScope, ts.peek().line)) return false; // check and add to symbol table
    addSymbolTableNode(name, "procedure", DataTypes::None, 0, globalScope);
    if (!ts.expect(TokenType::L_PAREN, "(")) return false;
    if (ts.isMatching(TokenType::VOID_STATEMENT)) {
    } else {
        if (!parseParamDeclList(ts)) return false;
    }
    if (!ts.expect(TokenType::R_PAREN, ")")) return false;
    basicAST.push_back(ASTElements::DECLARATION);
    bool ok = parseBlock(ts);
    globalScope = 0;
    return ok;
}

enum class ParseResult { // used to determine if top level items do not exist or if they had an error
    None,
    Ok,
    Error
};

ParseResult parseTopLevelItem(TokenStream& ts) {
    if (ts.peek().type == TokenType::FUNCTION_DECLARATION) {
        if (!parseFunction(ts)) return ParseResult::Error;
        return ParseResult::Ok;
    }
    if (ts.peek().type == TokenType::PROCEDURE_DECLARATION &&
        ts.peek(1).type == TokenType::IDENTIFIER) {
        if (!parseProcedureDeclaration(ts)) return ParseResult::Error;
        return ParseResult::Ok;
        }
    if (ts.peek().type == TokenType::CHAR_STATEMENT ||
        ts.peek().type == TokenType::BOOL_STATEMENT ||
        ts.peek().type == TokenType::INT_STATEMENT) {
        if (!parseDeclaration(ts)) return ParseResult::Error;
        return ParseResult::Ok;
        }
    return ParseResult::None;
}

bool parseProgram(TokenStream& ts) {
    ParseResult result;
    while (true) { // check for functions, non-main procedures, and global variables
        result = parseTopLevelItem(ts);
        if (result == ParseResult::Error) return false; // stop on real syntax error
        if (result == ParseResult::None) break;         // stop normally
    }
    if (!parseMainProcedure(ts)) return false; // ensure there is a main
    while (true) { // check for functions, non-main procedures, and global variables
        result = parseTopLevelItem(ts);
        if (result == ParseResult::Error) return false; // stop on real syntax error
        if (result == ParseResult::None) break;         // stop normally
    }
    if (ts.peek().type != TokenType::END) {
        std::cerr << "Syntax error near line " << ts.peek().line << ": unexpected tokens after program end\n";
        return false;
    }
    return true;
}

int main( int argc, char *argv[] ) {
    // Check for an input file and test opening it
    if( argc != 2 ) {
        std::cout << "No file included. Please include a file.\n";
        exit(5);
    }
    std::fstream inputStream;
    inputStream.open(argv[1], std::ios::in);
    if(!inputStream.is_open()) {
        std::cout << "Unable to open the input file " <<
                  argv[1] << std::endl;
        std::cout << "Terminating...\n";
        exit(7);
    }

    auto [successful, noComments] = removeComments(inputStream);
    if (successful) {
        std::istringstream inFile(noComments);
        if (tokenize(inFile)) {
            TokenStream ts{tokenList};

            if (parseProgram(ts)) {
                if (!head) {
                    std::cerr << "Unable to build Symbol Table.\n";
                    return 1;
                }
                std::vector<ASTElements> test = basicAST;
                CSTNode* AST = buildAST(test, tokenList);
                printCST(AST);
                //printSymbolTable(head);
                /*CSTNode* root = buildCSTFromTokens(tokenList);

                if (!root) {
                    std::cerr << "Unable to build CST.\n";
                    return 1;
                }

                printCST(root);*/
            }
        }
    } else {
        std::cout << "Unable to remove the comments.";
    }


    inputStream.close();
    return 0;
}