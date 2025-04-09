import re
from lexer import Lexer, Token, TokenType
from collections import deque
from json_parsers import JSONNumber, JSONString


class Parser:
    tokenList: list[Token]
    stackTrace: deque

    # Initializes a Parser with a tokenlist.
    def __init__(self, tokenList: list[Token]):
        self.tokenList = tokenList
        self.stackTrace = deque()


if __name__ == "__main__":
    jsonStr = input().strip()
    # parser = Parser(Lexer().generateTokens(jsonStr))
    print(JSONString().ParseString(jsonStr))
