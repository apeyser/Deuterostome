/test {
  (Integer test ) toconsole
  gettime 0.0 0.0 1.0 1e8 { add } for pop gettime exch sub _ pop
  /x 10000 /d array def /y 10000 /d array def
  (Float test ) toconsole
  0.0 y copy pop 1.123123212 x copy pop
  gettime y 1000000 { x add } repeat pop gettime exch sub _ pop
  (Matrix test ) toconsole
  /x 1e6 /d array def
  gettime 1000 { 17.4 x copy 19.5 mul pop } repeat gettime exch sub _ pop
} bind def
