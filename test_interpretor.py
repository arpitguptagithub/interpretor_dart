import unittest


class LexerTestCase(unittest.TestCase):
    def makeLexer(self, text):
        from spi import Lexer
        lexer = Lexer(text)
        return lexer

    def test_tokens(self):
        from spi import TokenType
        records = (
            ('234', TokenType.INT_CONST, 234),
            ('3.14', TokenType.DOUBLE_CONST, 3.14),
            ('*', TokenType.MUL, '*'),
    
            ('/', TokenType.DIV, '/'),
            ('+', TokenType.PLUS, '+'),
            ('-', TokenType.MINUS, '-'),
            ('(', TokenType.LPAREN, '('),
            (')', TokenType.RPAREN, ')'),
            ('=', TokenType.ASSIGN, '='),
            ('.', TokenType.DOT, '.'),
            ('number', TokenType.ID, 'number'),
            (';', TokenType.SEMI, ';'),
            ('{', TokenType.LBRACE, '{'),
            ('}', TokenType.RBRACE, '}'),
            ('PROCEDURE', TokenType.PROCEDURE, 'PROCEDURE'),
        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_lexer_exception(self):
        from spi import LexerError
        lexer = self.makeLexer('<')
        with self.assertRaises(LexerError):
            lexer.get_next_token()


class ParserTestCase(unittest.TestCase):
    def makeParser(self, text):
        from spi import Lexer, Parser
        lexer = Lexer(text)
        parser = Parser(lexer)
        return parser

    def test_expression_invalid_syntax_01(self):
        from spi import ParserError, ErrorCode
        parser = self.makeParser(
            """
            VOID Test()
            VAR
                int a;
            {
               a = 10 * ;  {Invalid syntax}
            }
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, ';')
        self.assertEqual(the_exception.token.lineno, 6)

    def test_expression_invalid_syntax_02(self):
        from spi import ParserError, ErrorCode
        parser = self.makeParser(
            """
            VOID Test()
            VAR
                 int a ;
            {
               a = 1 (1 + 2); {Invalid syntax}
            }
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, '(')
        self.assertEqual(the_exception.token.lineno, 6)

    def test_maximum_one_VAR_block_is_allowed(self):
        from spi import ParserError, ErrorCode
        # zero VARs
        parser = self.makeParser(
            """
            VOID Test()
            {
            }
            """
        )
        parser.parse()

        # one VAR
        parser = self.makeParser(
            """
            VOID Test()
            VAR
                int a ;
            {
            }
            """
        )
        parser.parse()

        parser = self.makeParser(
            """
            VOID Test()
            VAR
                int a  ;
            VAR
                int b ;
            {
               a = 5;
               b = a + 10;
            }
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, 'VAR')
        self.assertEqual(the_exception.token.lineno, 5)  # second VAR


class SemanticAnalyzerTestCase(unittest.TestCase):
    def runSemanticAnalyzer(self, text):
        from spi import Lexer, Parser, SemanticAnalyzer
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        return semantic_analyzer

    def test_semantic_duplicate_id_error(self):
        from spi import SemanticError, ErrorCode
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
            """
            VOID Test()
            VAR
                 int a ;
                 double a;  
            {
               a = 5;
            }
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.DUPLICATE_ID)
        self.assertEqual(the_exception.token.value, 'a')
        self.assertEqual(the_exception.token.lineno, 5)

    def test_semantic_id_not_found_error(self):
        from spi import SemanticError, ErrorCode
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
            """
            VOID Test()
            VAR
                int a ;
            {
               a = 5 + b;
            }
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.ID_NOT_FOUND)
        self.assertEqual(the_exception.token.value, 'b')


class TestCallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        # do nothing
        pass

    def peek(self):
        return self._records[-1]


class InterpreterTestCase(unittest.TestCase):
    def makeInterpreter(self, text):
        from spi import Lexer, Parser, SemanticAnalyzer, Interpreter
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)

        interpreter = Interpreter(tree)
        interpreter.call_stack = TestCallStack()
        return interpreter

    def test_integer_arithmetic_expressions(self):
        for expr, result in (
            ('3', 3),
            ('2 + 7 * 4', 30),
            ('7 - 8 / 4', 5),
            ('14 + 2 * 3 - 6 / 2', 17),
            ('7 + 3 * (10 / (12 / (3 + 1) - 1))', 22),
            ('7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)', 10),
            ('7 + (((3 + 2)))', 12),
            ('- 3', -3),
            ('+ 3', 3),
            ('5 - - - + - 3', 8),
            ('5 - - - + - (3 + 4) - +2', 10),
        ):
            interpreter = self.makeInterpreter(
                """VOID Test()
                   VAR
                       int a ;
                   {
                       a = %s
                   }
                """ % expr
            )
            interpreter.interpret()
            ar = interpreter.call_stack.peek()
            self.assertEqual(ar['a'], result)

    def test_float_arithmetic_expressions(self):
        for expr, result in (
            ('3.14', 3.14),
            ('2.14 + 7 * 4', 30.14),
            ('7.14 - 8 / 4', 5.14),
        ):
            interpreter = self.makeInterpreter(
                """VOID Test()
                   VAR
                       double a;
                   {
                       a = %s
                   }
                """ % expr
            )
            interpreter.interpret()
            ar = interpreter.call_stack.peek()
            self.assertEqual(ar['a'], result)

    def test_procedure_call(self):
        text = """
VOID Main()

procedure Alpha(int a ; int b)
var int a;
{
   x = (a + b ) * 2;
}

{  /*ain0*/

   Alpha(3 + 5, 7);

}  /* Main */
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar['a'], 8)
        self.assertEqual(ar['b'], 7)
        self.assertEqual(ar['x'], 30)
        self.assertEqual(ar.nesting_level, 2)

    def test_program(self):
        text = """\
VOID Part12()
VAR
   int number ;
   int  a, b   ;
   double y ;

PROCEDURE P1
VAR
   double a;
    int k ;
   PROCEDURE P2
   VAR
      int a, z  ;
   { 
      z = 777;
   } 
{ 

}  

{ 
   number = 2;
   a = number ;
   b = 10 * a + 10 * number / 4;
   y = 20 / 7 + 3.14
}  /*Part12*/
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(len(ar.members.keys()), 4)
        self.assertEqual(ar['number'], 2)
        self.assertEqual(ar['a'], 2)
        self.assertEqual(ar['b'], 25)
        self.assertAlmostEqual(ar['y'], float(20) / 7 + 3.14)  # 5.9971...


if __name__ == '__main__':
    unittest.main()