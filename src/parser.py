import re
from lexer import Lexer, Token, TokenType


class Parser:
    tokenList: list[Token]

    # Initializes a Parser with a tokenlist.
    def __init__(self, tokenList: list[Token]):
        self.tokenList = tokenList

    # Returns whether the set of tokens produce a valid JSON.
    def isJSON() -> bool:
        pass


if __name__ == "__main__":
    jsonStr = input().strip()
    parser = Parser(Lexer().generateTokens(jsonStr))

    for token in parser.tokenList:
        print(token)
