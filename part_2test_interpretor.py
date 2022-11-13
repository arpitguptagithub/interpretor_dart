import unittest


class LexerTestCase(unittest.TestCase):
    def makeLexer(self, text):
        from spi import Lexer
        lexer = Lexer(text)
        return lexer

    def test_lexer_integer(self):
        from spi import INTEGER
        lexer = self.makeLexer('234')
        token = lexer.get_next_token()
        self.assertEqual(token.type, INTEGER)
        self.assertEqual(token.value, 234)

    def test_lexer_mul(self):
        from spi import MUL
        lexer = self.makeLexer('*')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MUL)
        self.assertEqual(token.value, '*')

    def test_lexer_div(self):
        from spi import DIV
        lexer = self.makeLexer(' / ')
        token = lexer.get_next_token()
        self.assertEqual(token.type, DIV)
        self.assertEqual(token.value, '/')
        
    def test_lexer_div(self):
        from spi import MOD
        lexer = self.makeLexer('%')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MOD)
        self.assertEqual(token.value, '%')

    def test_lexer_plus(self):
        from spi import PLUS
        lexer = self.makeLexer('+')
        token = lexer.get_next_token()
        self.assertEqual(token.type, PLUS)
        self.assertEqual(token.value, '+')

    def test_lexer_minus(self):
        from spi import MINUS
        lexer = self.makeLexer('-')
        token = lexer.get_next_token()
        self.assertEqual(token.type, MINUS)
        self.assertEqual(token.value, '-')

    def test_lexer_lparen(self):
        from spi import LPAREN
        lexer = self.makeLexer('(')
        token = lexer.get_next_token()
        self.assertEqual(token.type, LPAREN)
        self.assertEqual(token.value, '(')

    def test_lexer_rparen(self):
        from spi import RPAREN
        lexer = self.makeLexer(')')
        token = lexer.get_next_token()
        self.assertEqual(token.type, RPAREN)
        self.assertEqual(token.value, ')')

    def test_lexer_new_tokens(self):
        from spi import ASSIGN, ID, SEMI, LBRACE, RBRACE,LPAREN,RPAREN,VOID
        records = (
            ('=', ASSIGN, '='),
            ('number', ID, 'number'),
            (';', SEMI, ';'),
            ('{', LBRACE, '{'), 
            ('}', RBRACE, '}'),
             ('(', LPAREN, '('),
            (')', RPAREN, ')'),
            ('VOID', VOID,'VOID')

        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)


class InterpreterTestCase(unittest.TestCase):
    def makeInterpreter(self, text):
        from spi import Lexer, Parser, Interpreter
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        return interpreter

    def test_arithmetic_expressions(self):
        for expr, result in (
           
            ('5 + 3', 8),
            ('5 +  (3 + 4) - 2', 10),
        ):
            interpreter = self.makeInterpreter('VOID MAIN(){  a = %s }' % expr)
            interpreter.interpret()
            globals = interpreter.GLOBAL_SCOPE
            self.assertEqual(globals['a'], result)

    def test_expression_invalid_syntax1(self):
        interpreter = self.makeInterpreter(' VOID MAIN(){ a = 10*  ; }')
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_expression_invalid_syntax2(self):
        interpreter = self.makeInterpreter('VOID MAIN(){  a = 1 (1 + 2); }')
        with self.assertRaises(Exception):
            interpreter.interpret()

    def test_statements(self):
        text = """\
VOID MAIN()

{

    
        number = 2;
        a = number;
        b = 10 * a + 10 * number / 4;
        c = a - b;
       

/*AAA*/
    x = 11;

}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        globals = interpreter.GLOBAL_SCOPE
        self.assertEqual(len(globals.keys()), 5)
        self.assertEqual(globals['number'], 2)
        self.assertEqual(globals['a'], 2)
        self.assertEqual(globals['b'], 25)
        self.assertEqual(globals['c'], -23)
        self.assertEqual(globals['x'], 11)
        


if __name__ == '__main__':
    unittest.main()