from enum import Enum
import re


class TokenType(Enum):
    ALPHANUM = 0
    COMMA = 1
    LPARANTH = 2
    RPARANTH = 3
    NULL = 4
    SPACE = 5
    COLON = 6
    LIST_START = 7
    LIST_END = 8
    ESCAPE_SEQUENCE = 9
    DQUOTES = 10
    OTHER = 11


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
    jsonText: str
    currPos: int
    tokenList: list[Token]

    # Initializes the Lexer class with the text. Also initialize the current position no.
    def __init__(self) -> None:
        self.jsonText = ""
        self.currPos = -1
        self.tokenList = list()

    # Sets Lexer Text.
    def setText(self, text: str) -> None:
        self.jsonText = text

    # Have an iterator to the next character.
    def advance(self) -> None:
        self.currPos += 1

    # Inputs a Text, and returns a list of tokens.
    def generateTokens(self) -> list[Token]:

        for ch in self.jsonText:
            pass
        return self.tokenList
