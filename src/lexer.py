from enum import Enum
import re
from collections import deque


# Enum to represent different token types in a JSON string
class TokenType(Enum):
    LEFT_BRACE = "{"
    DQUOTES = '"'
    LEFT_BRACKET = "["
    COMMA = ","
    COLON = ":"
    RIGHT_BRACE = "}"
    RIGHT_BRACKET = "]"
    JSTRING = ""
    WS = " "
    ESCAPE_CHAR = "\\"
    TRUE = "true"
    FALSE = "false"
    NULL = "null"
    NEWLINE = "\n"
    TABLINE = "\t"


# Class representing a token with type, value, and indentation level
class Token:
    type: TokenType
    value: str
    level: int
    line: int

    def __init__(self, type: TokenType, value: str, level: int = 0) -> None:
        """
        Initializes a Token object.

        Args:
            type (TokenType): The type of the token.
            value (str): The actual string value of the token.
            level (int): The nesting level of the token (used for depth tracking).
        """
        self.type = type
        self.value = value
        self.level = level

    @staticmethod
    def GetTextColor(type: TokenType) -> str:
        """
        Returns ANSI color code for a given token type.

        Args:
            type (TokenType): The token type.

        Returns:
            str: ANSI color string.
        """
        match type:
            case TokenType.LEFT_BRACE:
                return "\033[31m"
            case TokenType.RIGHT_BRACE:
                return "\033[31m"
            case TokenType.LEFT_BRACKET:
                return "\033[33m"
            case TokenType.RIGHT_BRACKET:
                return "\033[33m"
            case TokenType.TRUE:
                return "\033[36m"
            case TokenType.FALSE:
                return "\033[36m"
            case TokenType.NULL:
                return "\033[36m"
            case TokenType.DQUOTES:
                return "\033[34m"
            case TokenType.COMMA:
                return "\033[38;5;213m"
            case TokenType.COLON:
                return "\033[38;5;208m"
            case TokenType.JSTRING:
                return "\033[32m"
            case _:
                return "\033[0m"

    def __repr__(self) -> str:
        """
        Returns a formatted string representation of the token with color.

        Returns:
            str: Formatted and colored string.
        """
        textColor = Token.GetTextColor(self.type)
        resetColor = "\033[0m"
        return f"{textColor}Token Type :-> {self.type.name : ^13} , Level : {self.level : ^2}, Value : {self.value}{resetColor}"


# JSONLexer generates tokens from input JSON string
class JSONLexer:
    tokenList: list[Token]
    lexStack: deque
    jsonText: str

    def __init__(self, jsonText: str) -> None:
        """
        Initializes the JSONLexer.

        Args:
            jsonText (str): The input JSON string.
        """
        self.tokenList = list()
        self.lexStack = deque()
        self.jsonText = jsonText

    def generateTokens(self) -> None:
        """
        Processes the input JSON string and generates tokens.
        Populates tokenList with Token instances.
        """
        start = 0
        curr = 0
        insideQuotes = False
        self.tokenList.clear()
        self.lexStack.clear()

        while curr < len(self.jsonText):
            currChar = self.jsonText[curr]

            # Matching individual characters
            match currChar:
                case TokenType.NEWLINE.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        curr += 1

                case TokenType.TABLINE.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        curr += 1

                case TokenType.LEFT_BRACE.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.lexStack.append(TokenType.LEFT_BRACE)
                        self.tokenList.append(
                            Token(TokenType.LEFT_BRACE, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.LEFT_BRACKET.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.lexStack.append(TokenType.LEFT_BRACKET)
                        self.tokenList.append(
                            Token(TokenType.LEFT_BRACKET, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.COMMA.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.tokenList.append(
                            Token(TokenType.COMMA, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.COLON.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.tokenList.append(
                            Token(TokenType.COLON, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.RIGHT_BRACE.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.tokenList.append(
                            Token(TokenType.RIGHT_BRACE, currChar, len(self.lexStack))
                        )
                        if len(self.lexStack) > 0:
                            self.lexStack.pop()
                        curr += 1
                        start = curr

                case TokenType.RIGHT_BRACKET.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        self.tokenList.append(
                            Token(TokenType.RIGHT_BRACKET, currChar, len(self.lexStack))
                        )
                        if len(self.lexStack) > 0:
                            self.lexStack.pop()
                        curr += 1
                        start = curr

                case TokenType.DQUOTES.value:
                    self.tokenList.append(
                        Token(TokenType.DQUOTES, currChar, len(self.lexStack))
                    )
                    insideQuotes = not insideQuotes
                    curr += 1

                case TokenType.WS.value:
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        curr += 1
                        start = curr

                case _:
                    # Handle TRUE/FALSE/NULL keywords
                    if (
                        not insideQuotes
                        and curr + 4 < len(self.jsonText)
                        and self.jsonText[curr : curr + 4] == TokenType.TRUE.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.TRUE, True, len(self.lexStack))
                        )
                        curr += 4
                        start = curr

                    elif (
                        not insideQuotes
                        and curr + 5 < len(self.jsonText)
                        and self.jsonText[curr : curr + 5] == TokenType.FALSE.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.FALSE, False, len(self.lexStack))
                        )
                        curr += 5
                        start = curr

                    elif (
                        not insideQuotes
                        and curr + 4 < len(self.jsonText)
                        and self.jsonText[curr : curr + 4] == TokenType.NULL.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.NULL, None, len(self.lexStack))
                        )
                        curr += 4
                        start = curr

                    else:
                        # Handle regular characters and escaped sequences
                        if currChar != TokenType.ESCAPE_CHAR.value:
                            if (
                                len(self.tokenList) > 0
                                and self.tokenList[-1].type == TokenType.JSTRING
                            ):
                                self.tokenList[-1].value += currChar
                                curr += 1
                            else:
                                self.tokenList.append(
                                    Token(
                                        TokenType.JSTRING, currChar, len(self.lexStack)
                                    )
                                )
                                start = curr
                                curr += 1
                        else:
                            # Handle backslash escape sequences
                            if curr + 1 < len(self.jsonText):
                                currChar += self.jsonText[curr + 1]
                            if (
                                len(self.tokenList) > 0
                                and self.tokenList[-1].type == TokenType.JSTRING
                            ):
                                self.tokenList[-1].value += currChar
                                curr += len(currChar)
                            else:
                                self.tokenList.append(
                                    Token(
                                        TokenType.JSTRING, currChar, len(self.lexStack)
                                    )
                                )
                                start = curr
                                curr += len(currChar)

    def printTokenList(self) -> None:
        """
        Prints all tokens with formatting and colors.
        """
        print(
            "==================================== LEXER TOKENLIST OUTPUT ====================================\n\n"
        )
        for i, token in enumerate(self.tokenList):
            print(f"Index {i : ^3} : {token}")
        print("\n")
