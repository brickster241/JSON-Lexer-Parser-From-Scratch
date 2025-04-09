from enum import Enum
import re


class TokenType(Enum):
    GENERAL = r"(?:[a-zA-Z0-9@!#$%^&*\(\)/\-_=+\.\\])+"
    COMMA = r","
    LPARANTH = r"\{"
    RPARANTH = r"\}"
    SPACE = r" "
    COLON = r":"
    LIST_START = r"\["
    LIST_END = r"\]"
    ESCAPE_SEQUENCE = r"(?:\\[stn\\])"
    DQUOTES = r'"'


class Token:
    type: TokenType
    value: str

    # Initializes a Token while parsing through the string.
    def __init__(self, type: TokenType, value: str) -> None:
        self.type = type
        self.value = value

    # Utility function to print a token.
    def __repr__(self) -> str:
        return f"Token : '{self.value}' -> Type : {self.type}"


class Lexer:
    tokenList: list[Token]

    # Initializes the Lexer class with the text. Also initialize the current position no.
    def __init__(self) -> None:
        self.tokenList = list()

    # Inputs a Text, and returns a list of tokens.
    def generateTokens(self, jsonText: str) -> list[Token]:
        currPos = 0
        while currPos < len(jsonText):
            ch = jsonText[currPos]
            if re.match(TokenType.GENERAL.value, ch):
                if (
                    len(self.tokenList) > 0
                    and self.tokenList[-1].type == TokenType.GENERAL
                ):
                    self.tokenList[-1].value = self.tokenList[-1].value + ch
                else:
                    self.tokenList.append(Token(TokenType.GENERAL, ch))
            elif re.match(TokenType.LPARANTH.value, ch):
                self.tokenList.append(Token(TokenType.LPARANTH, ch))
            elif re.match(TokenType.RPARANTH.value, ch):
                self.tokenList.append(Token(TokenType.RPARANTH, ch))
            elif re.match(TokenType.LIST_START.value, ch):
                self.tokenList.append(Token(TokenType.LIST_START, ch))
            elif re.match(TokenType.LIST_END.value, ch):
                self.tokenList.append(Token(TokenType.LIST_END, ch))
            elif re.match(TokenType.DQUOTES.value, ch):
                self.tokenList.append(Token(TokenType.DQUOTES, ch))
            elif re.match(TokenType.COLON.value, ch):
                self.tokenList.append(Token(TokenType.COLON, ch))
            elif re.match(TokenType.SPACE.value, ch):
                self.tokenList.append(Token(TokenType.SPACE, ch))
            currPos += 1

        return self.tokenList
