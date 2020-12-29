(package json []

(define read-all-chars
  Stream -> (let C (read-byte Stream)
              (if (= C -1)
                []
                [C | (read-all-chars Stream)])))

(define file->chars
  Name -> (let Stream (open Name in) (read-all-chars Stream)))

(define collect-string
  [34 | Rest] -> [34 | (remove-spaces Rest)]
  [C | Rest]  -> [C | (collect-string Rest)])

(define remove-spaces
  []          -> []
  [32 | Rest] -> (remove-spaces Rest)
  [10 | Rest] -> (remove-spaces Rest)
  [34 | Rest] -> [34 | (collect-string Rest)]
  [C | Rest]  -> [C | (remove-spaces Rest)])

(define parse-file
  Name -> (compile (function <value>) (remove-spaces (file->chars Name))))

(defcc <char>
  Char := Char;)

(defcc <space>
  32 := " ";)

(defcc <doublequote>
  34 := (n->string 34);)

(defcc <backslash>
  92 := "\";)

(defcc <u>
  117 := "u";)

(defcc <escape-char>
  34  := (n->string 34);
  92  := "\";
  47  := "/";
  98  := "b";
  102 := "f";
  110 := "n";
  114 := "r";
  116 := "t";)

(defcc <escaped>
  <backslash> <escape-char> := (@s <backslash> <escape-char>);)

(defcc <hex>
  <num> := <num>;
  65    := "A";
  66    := "B";
  67    := "C";
  68    := "D";
  69    := "E";
  70    := "F";
  97    := "a";
  98    := "b";
  99    := "c";
  100   := "d";
  101   := "e";
  102   := "f";)

(defcc <hex1>
  <hex> := <hex>;)

(defcc <hex2>
  <hex> := <hex>;)

(defcc <hex3>
  <hex> := <hex>;)

(defcc <hex4>
  <hex> := <hex>;)

(defcc <unicode>
  <backslash> <u> <hex1> <hex2> <hex3> <hex4> :=
    (@s <backslash> <u> <hex1> <hex2> <hex3> <hex4>);)

(defcc <string-char>
  <escaped> := <escaped>;
  <unicode> := <unicode>;
  <char>    := (n->string <char>) where (not (= 34 <char>));)

(defcc <char-stream>
  <string-char> <char-stream> := (@s <string-char> <char-stream>);
  <string-char>               := <string-char>;
  <e>                         := "";)

(defcc <string>
  <doublequote> <char-stream> <doublequote> := <char-stream>;
  <doublequote> <doublequote>               := "";)

(define num
  48 -> 0
  49 -> 1
  50 -> 2
  51 -> 3
  52 -> 4
  53 -> 5
  54 -> 6
  55 -> 7
  56 -> 8
  57 -> 9
  _  -> -1)

(defcc <num>
  Num := (num Num) where (>= (num Num) 0);)

(defcc <digits>
  <num> <digits> := [<num> | <digits>];
  <num>          := [<num>];
  <e>            := [];)

(defcc <post-digits>
  <digits> := <digits>;)

(defcc <period>
  46 := ".";)

(define exp10
  [N | R] E -> (+ (* N E) (exp10 R (* E 10)))
  []      _ -> 0)

(define div10
  [N | R] E -> (+ (* N E) (div10 R (/ E 10)))
  []      _ -> 0)

(defcc <minus>
  45 := "-";)

(defcc <positive-number>
  <digits> <period> <post-digits> := (+ (exp10 (reverse <digits>) 1) (div10 <post-digits> 0.1));
  <digits>                        := (exp10 (reverse <digits>) 1);)

(defcc <number>
  <minus> <positive-number> := (- 0 <positive-number>);
  <positive-number>         := <positive-number>;)

(defcc <null>
  110 117 108 108 := null;)

(defcc <true>
  116 114 117 101 := true;)

(defcc <false>
  102 97 108 115 101 := false;)

(defcc <lbracket>
  91 := "[";)

(defcc <rbracket>
  93 := "]";)

(defcc <comma>
  44 := ",";)

(defcc <array-values>
  <value> <comma> <array-values> := [<value> | <array-values>];
  <value>                        := [<value>];
  <e>                            := [];)

(defcc <array>
  <lbracket> <rbracket>                := [];
  <lbracket> <array-values> <rbracket> := <array-values>;)

(defcc <lbrace>
  123 := "{";)

(defcc <rbrace>
  125 := "}";)

(defcc <colon>
  58 := ":";)

(defcc <dict-values>
  <string> <colon> <value> <comma> <dict-values> := [(@p <string> <value>) | <dict-values>];
  <string> <colon> <value>                       := [(@p <string> <value>)];
  <e>                                            := [];)

(defcc <dict>
  <lbrace> <rbrace>               := [];
  <lbrace> <dict-values> <rbrace> := <dict-values>;)

(defcc <value>
  <null>   := <null>;
  <false>  := <false>;
  <true>   := <true>;
  <string> := <string>;
  <array>  := <array>;
  <dict>   := <dict>;
  <number> := <number>;)

)