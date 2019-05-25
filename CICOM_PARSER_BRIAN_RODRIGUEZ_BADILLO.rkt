#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

;;;BRIAN RODRIGUEZ BADILLO
;;Cicom Parser


;;;CICOM LEX ABBREVIATIONS
(define-lex-abbrevs
  [character (:or (:/ "a" "z")(:/ #\A #\Z)  "?" "_")]
  [digit  (:/ #\0 #\9)]
  [space (:or #\tab #\space #\newline)]
  [delim (:or  "(" ")" "[" "]" "," ";")]
  [operator (:or  "~" "*" "/" "=" "!=" "<" ">" "<=" ">=" "&" ":=")]
  [keyword (:or "if" "then" "else" "let" "in" "map" "to"
               "empty" "true" "false")])

;;Define tokens to be used.
(define-tokens character-digit (CHARACTER DIGIT ID))
(define-tokens delimiters(OP CP OB CB C SC EOF))
(define-tokens operators( P M ~ * / = != < > <= >= & or :=))
(define-tokens keywords(if then else let in map to empty true false number?
                        function? list? empty? cons? cons first rest arity))


;;;CICOM TOKENIZER
(define tokenize               
      (lexer
       ;;Tokenize

       ;Skip tabs, spaces or newlines.
       [(:or space whitespace blank iso-control) (tokenize input-port)]                                       

       ;;Tokenize delimiters
       [(eof) 'EOF]                                               
       ["("(token-OP (string->symbol lexeme))]
       [")" (token-CP (string->symbol lexeme))]
       ["[" (token-OB (string->symbol lexeme))]
       ["]" (token-CB (string->symbol lexeme))]
       ["," (token-C (string->symbol lexeme))]
       [";" (token-SC (string->symbol lexeme))]

       ;;Tokenize operators
       [operator (string->symbol lexeme)]                         ;These strings will be mapped as symbols to use in the grammar.
       ["+" (token-P (string->symbol lexeme))]
       ["-" (token-M (string->symbol lexeme))]
       ["|" (token-or (string->symbol lexeme))]

       ;;Tokenize special keywords
       [keyword (string->symbol lexeme)]                          ;These strings will be mapped as symbols to use in the grammar.

       ;;Tokenize IDs and Digits
      [numeric (token-DIGIT (string->number lexeme))]
      [(:: character(:* (:or character digit)))
       (token-ID (string->symbol lexeme))]
  ))


;;;CICOM PARSER
(define cicom-parser
  (parser

   (start Exp)
   (end EOF)
   (tokens character-digit delimiters operators keywords)

   (error (lambda (tok-ok? tok-name tok-val)
            (cond
              [(equal? tok-name token-EOF) void]
              [else (print "error in ->")(print tok-name)(print":")(print tok-val)])))
                                              

   
   (grammar
    
    (Exp [(Term)"OK"] 
         [(Term Binop Exp) "OK"] 
         [(if Exp then Exp else Exp) "OK"]
         [(let Def in Exp)"OK"] 
         [(map IdList to Exp)"OK"])
    
    (Term [(Unop Term)"OK"]
          [(Factor OP ExpList CP) "OK"]
          [(Factor) "OK"]
          [(empty) "OK"]
          [(Int) "OK"]
          [(Bool) "OK"])
    
    (Factor [(OP Exp CP) "OK"]
            [(Prim) "OK"]
            [(ID) "OK"])
    
    (ExpList [(PropExpList) "OK"]
             [() "OK"])
    
    (PropExpList [(Exp) "OK"]
                 [(Exp C PropExpList) "OK"])
    
    (IdList [(PropIdList) "OK"]
            [() "OK"])
    
    (PropIdList [(ID) "OK"]
                [(ID C PropIdList) "OK"])
    
    (Def [(ID := Exp SC) "OK"]
         [(ID := Exp SC Def) "OK"])
    
    (Empty [(empty) "OK"])
    
    (Bool [(true) "OK"]
          [(false) "OK"])
    
    (Unop [(Sign) "OK"]
          [(~) "OK"])
   
    (Sign [(P) "OK"]
          [(M) "OK"])
    
   (Binop [(Sign) "OK"]
          [(*) "OK"]
          [(/) "OK"]
          [(=) "OK"]
          [(!=) "OK"]
          [(<) "OK"]
          [(>) "OK"]
          [(<=) "OK"]
          [(>=) "OK"]
          [(&) "OK"]
          [(or) "OK"])
   
   (Prim [(number?) "OK"]
         [(function?) "OK"]
         [(list?) "OK"]
         [(empty?) "OK"]
         [(cons?) "OK"]
         [(cons) "OK"]
         [(first) "OK"]
         [(rest) "OK"]
         [(arity) "OK"])

    (Int [(DIGIT) "OK"])
    )))

;;Define the call that parse
(define (lex-this lexer input)
  (lambda () (lexer input)))

(define (cicom-parse input_file)
  (cicom-parser
   (lex-this tokenize (open-input-file input_file))))

;;Autorun for Test.txt
(cicom-parse "Test.txt")