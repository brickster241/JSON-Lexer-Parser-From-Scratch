from enum import Enum
import re
from collections import deque
from json_validators import JSONNumberLiteral, JSONStringLiteral


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


class Token:
    type: TokenType
    value: str
    level: int

    # Initializes a Token while parsing through the string.
    def __init__(self, type: TokenType, value: str, level: int = 0) -> None:
        self.type = type
        self.value = value
        self.level = level

    # Utility function which extracts unique text color for printing showing clarity.
    def GetTextColor(self) -> str:
        match self.type:
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

    # Utility function to print a token.
    def __repr__(self) -> str:
        textColor = self.GetTextColor()
        resetColor = "\033[0m"
        return f"{textColor}Token Type :-> {self.type.name : ^13} , Level : {self.level : ^2}, Value : {self.value}{resetColor}"


class JSONLexer:
    tokenList: list[Token]
    lexStack: deque

    # Initializes the Lexer class with the text. Also initialize the current position no.
    def __init__(self) -> None:
        self.tokenList = list()
        self.lexStack = deque()

    # Inputs a Text, and returns a list of tokens.
    def generateTokens(self, jsonText: str) -> None:
        start = 0
        curr = 0
        insideQuotes = False
        self.tokenList.clear()
        self.lexStack.clear()
        while curr < len(jsonText):
            currChar = jsonText[curr]
            match currChar:
                case TokenType.LEFT_BRACE.value:
                    # Either you are part of string or starting a new object.
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
                    # Either you are part of string or starting a new array.
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
                    # Either you are part of string, or seperating two different values in array or JSON.
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        # SUCCESSFULLY SEPERATES.
                        self.tokenList.append(
                            Token(TokenType.COMMA, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.COLON.value:
                    # Either you are part of string , or seperating key value pair.
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        # SUCCESSFULLY SEPERATES.
                        self.tokenList.append(
                            Token(TokenType.COLON, currChar, len(self.lexStack))
                        )
                        curr += 1
                        start = curr

                case TokenType.RIGHT_BRACE.value:
                    # Either you are part of string or closing a new object.
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
                    # Either you are part of string or closing a new object.
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
                    # Will only enter this case when we need to invert quotes.
                    self.tokenList.append(
                        Token(TokenType.DQUOTES, currChar, len(self.lexStack))
                    )
                    insideQuotes = not insideQuotes
                    curr += 1

                case TokenType.WS.value:
                    # Either it is inside a string, in that case you should not skip the spaces, else skip.
                    if insideQuotes:
                        curr += 1
                        self.tokenList[-1].value += currChar
                    else:
                        curr += 1
                        start = curr

                case _:
                    # TRUE, FALSE AND NULL KEYWORDS
                    if (
                        not insideQuotes
                        and curr + 4 < len(jsonText)
                        and jsonText[curr : curr + 4] == TokenType.TRUE.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.TRUE, True, len(self.lexStack))
                        )
                        curr += 4
                        start = curr
                    elif (
                        not insideQuotes
                        and curr + 5 < len(jsonText)
                        and jsonText[curr : curr + 5] == TokenType.FALSE.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.FALSE, False, len(self.lexStack))
                        )
                        curr += 5
                        start = curr
                    elif (
                        not insideQuotes
                        and curr + 4 < len(jsonText)
                        and jsonText[curr : curr + 4] == TokenType.NULL.value
                    ):
                        self.tokenList.append(
                            Token(TokenType.NULL, None, len(self.lexStack))
                        )
                        curr += 4
                        start = curr
                    else:
                        # REST ALL CHARACTERS WILL BE ADDED, JUST REMEMBER TO CHECK FOR ADDITIONAL CHARACTER AFTER \
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
                            # CURR CHARACTER IS A BACKSLASH.
                            if curr + 1 < len(jsonText):
                                currChar += jsonText[curr + 1]
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
