/BENCHMARK module

200 dict dup begin

/darrsize 1e7 /l ctype def
/darr darrsize /d array 0 darrsize 0 1 ramp pop def

/sarrsize 1e7 /l ctype def
/sarr sarrsize /d array 0 sarrsize 0 1 ramp pop def


/tests [
    [ (Array double add\n)
        {
            0d 200 {darr add} repeat _ pop
        } bind
    ]
    [ (Array single add\n)
        {
            0s 200 {sarr add} repeat _ pop
        } bind
    ]
] def

/runtests {
    (Running tests:\n) toconsole
    tests {
        dup 0 get toconsole 
            1 get
        gettime exch exec gettime exch sub (Time: ) toconsole _ pop
    } forall
} bind def

end _module
