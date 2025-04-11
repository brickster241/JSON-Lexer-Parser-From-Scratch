from typing import Optional
from lexer import TokenType, Token
import re


class JSONParser:
    tokens: list[Token]
    pos: int
    MAX_DEPTH: int = 20

    # Initialize the Token List received from Lexer.
    def __init__(self, tokenList: list[Token]) -> None:
        self.tokens = tokenList
        self.pos = 0

    def parse(self):
        try:
            # First Character should always be a LEFT_BRACE or LEFT_BRACKET
            if self.peek().type not in [TokenType.LEFT_BRACE, TokenType.LEFT_BRACKET]:
                leftBraceTextColor = Token.GetTextColor(TokenType.LEFT_BRACE)
                leftBracketTextColor = Token.GetTextColor(TokenType.LEFT_BRACKET)
                tokenTextColor = Token.GetTextColor(self.peek().type)
                resetColor = "\033[0m"
                raise SyntaxError(
                    f"Expected {leftBraceTextColor}{TokenType.LEFT_BRACE}{resetColor} or {leftBracketTextColor}{TokenType.LEFT_BRACKET}{resetColor} ; but got {tokenTextColor}{self.peek().type}{resetColor} !!"
                )

            # TRY AND PARSE STUFF NOW.
            return self.parse_value()
        except SyntaxError as err:
            print(err)
            return None

    def parse_value(self):
        currToken = self.peek()
        match currToken.type:
            case TokenType.LEFT_BRACE:
                return self.parse_object()
            case TokenType.LEFT_BRACKET:
                return self.parse_array()
            case TokenType.NULL:
                self.pos += 1
                return None
            case TokenType.TRUE:
                self.pos += 1
                return True
            case TokenType.FALSE:
                self.pos += 1
                return False
            case TokenType.DQUOTES:
                return self.parse_string()
            case TokenType.JSTRING:
                return self.parse_number()

    def parse_array(self):
        arr = []
        self.consume(TokenType.LEFT_BRACKET)
        pass

    def parse_object(self):
        obj = dict()
        self.consume(TokenType.LEFT_BRACE)
        pass

    def parse_number(self):
        """
        Validates if the given string is a valid JSON number.

        Args:
            text (str): The input string to be validated.

        Returns:
            bool: True if the string matches the JSON number format, else False.
        """
        numberText = self.peek().value
        # Regex for a valid JSON number
        REGEX_STRING = r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?"

        # Use re.fullmatch to ensure the entire string matches the regex
        if re.fullmatch(REGEX_STRING, numberText):
            self.pos += 1
            if "." in numberText or "e" in numberText.lower():
                return float(numberText)
            else:
                return int(numberText)
        else:
            raise SyntaxError(f"[NUMBER MISMATCH ERROR] Value is not a valid number !!")

    def parse_string(self):
        """
        Validates if the given string is a valid JSON string.

        Args:
            text (str): The input string to be validated.

        Returns:
            bool: True if the string matches the JSON string format, else False.
        """
        self.consume(TokenType.DQUOTES)
        stringText = self.peek().value
        # Regex for a valid JSON string:
        REGEX_STRING = r'([^\\]|\\[bfnrt\\/"]|\\u[0-9a-fA-F]{4})*'

        # Use re.fullmatch to ensure the full string matches the pattern
        if re.fullmatch(REGEX_STRING, stringText):
            self.pos += 1
            self.consume(TokenType.DQUOTES)
            return stringText
        else:
            raise SyntaxError(f"[STRING MISMATCH ERROR] Value is not a valid string !!")

    def peek(self):
        if self.pos >= len(self.tokens):
            raise SyntaxError("[END_OF_INPUT_ERROR] Unexpected end of input !!")
        if self.tokens[self.pos].level >= self.MAX_DEPTH:
            raise SyntaxError(
                "[MAX_DEPTH_EXCEEDED_ERROR] JSON Nested Too deep, cannot be more than 20 !!"
            )
        return self.tokens[self.pos]

    def expect_end(self):
        if self.pos != len(self.tokens):
            raise SyntaxError("[TRAILING_TOKENS_ERROR] Unexpected trailing tokens !!")

    def consume(self, expected_type: TokenType) -> Token:
        token = self.tokens[self.pos]
        expectedTextColor = Token.GetTextColor(expected_type)
        tokenTextColor = Token.GetTextColor(token.type)
        resetColor = "\033[0m"
        if token.type != expected_type:
            raise SyntaxError(
                f"[UNEXPECTED_TOKEN_ERROR] Expected {expectedTextColor}{expected_type}{resetColor} but got {tokenTextColor}{token.type}{resetColor} !!"
            )
        if token.level >= self.MAX_DEPTH:
            raise SyntaxError(
                "[MAX_DEPTH_EXCEEDED_ERROR] JSON Nested Too deep, cannot be more than 20 !!"
            )
        self.pos += 1
        return token
