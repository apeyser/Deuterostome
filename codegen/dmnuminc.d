|============== D machine for PPC: DMNUMINC generator ==================
|
| This acts as an extended macro preprocessor to the C compiler, which
| prepares nested, conditional, multi-line constructs in C.
|

1000 dict dup begin

/__   { * exch text } def
/st__ { st mkact __ } def
/dt__ { dt mkact __ } def
/op__ { op mkact __ } def
/NL   { * (\r) text } def


|----- macro argument stack routines

/A_stack 20 list def
/A_free 0 def

/pushA { 
  A_stack A_free put /A_free A_free 1 add def
} bind def

/popA {
  /A_free A_free 1 sub def A_stack A_free get
} bind def

|----------------------------- macros: make object value pointer
| (frame) | --    

/SCALAR { pushA (NUM_VAL\()__    popA __ (\))__ } def
/ARRAY  { pushA (VALUE_BASE\()__ popA __ (\))__ } def

|----------------------------- macro: get value via pointer (using cast),
| convert it from type into into double type, and save it in temporary
| variable; propagate an undefined value.

| /type { pointer_constructor } (tempname) | --    

/GETCV { dup pushA exch pushA pushA  GETCV_d exch get exec } def

/GETCV_d 6 dict dup begin

/B { NL (if \(\()__ popA __ ( = *\(\(B *\))__ popA exec (\)\) == BINF\) )__
        popA __ ( = DINF;)__  
   } def
/W { NL (if \(\()__ popA __ ( = *\(\(W *\))__ popA exec (\)\) == WINF\) )__
        popA __ ( = DINF;)__  
   } def
/L { NL (if \(\()__ popA __ ( = *\(\(L *\))__ popA exec (\)\) == LINF\) )__
        popA __ ( = DINF;)__  
   } def
/S { NL popA __ ( = *\(\(S *\))__ popA exec (\);)__ popA pop
   } def
/D { NL popA __ ( = *\(\(D *\))__ popA exec (\);)__ popA pop
   } def
end def

|----------------------------- macro: build C code to convert a
| double value in a temporary variable into the destination type and
| to store the result at the destination:

|  /dest_type { dest_pointer_constructor } (tempname) | --    

/CVPUT { pushA pushA CVPUT_d exch get exec } def

/CVPUT_d 6 dict dup begin
/B { NL (*\(\(B *\))__ popA exec (\) = \(\(\()__
        popA dup pushA __ (\) > BMAX\) || \(\(-)__
        popA dup pushA __ (\) < -BMAX\) || \()__
        popA dup pushA __ ( == DINF\)\)? BINF : )__ popA __ (;)__
   } def
/W { NL (*\(\(W *\))__ popA exec (\) = \(\(\()__
        popA dup pushA __ (\) > WMAX\) || \(\(-)__
        popA dup pushA __ (\) < -WMAX\) || \()__
        popA dup pushA __ ( == DINF\)\)? WINF : )__ popA __ (;)__
   } def
/L { NL (*\(\(L *\))__ popA exec (\) = \(\(\()__
        popA dup pushA __ (\) > LMAX\) || \(\(-)__
        popA dup pushA __ (\) < -LMAX\) || \()__
        popA dup pushA __ ( == DINF\)\)? LINF : )__ popA __ (;)__
   } def
/S { NL (*\(\(S *\))__ popA exec (\) = )__ popA __ (;)__ 
   } def
/D { NL (*\(\(D *\))__ popA exec (\) = )__ popA __ (;)__ 
   } def
end def

|-------------------------------------------- construct C code of 'for'
| statement, which executes body as many times as array frame has
| elements (a long count variable, n, is assumed to exist);

| (framename) { body_constructor } | --

/FOR { pushA pushA
NL (for \(n = ARRAY_SIZE\()__ popA __ (\); n>0; n--\) {)__
popA exec
NL (})__
} def

/FOR_mod4 { pushA pushA
NL (for \(n = \(ARRAY_SIZE\()__ popA __ (\)>>2\); n>0; n--\) {)__
popA exec
NL (})__
} def

/FOR_mod1 { pushA pushA
NL (for \(n = \(ARRAY_SIZE\()__ popA __ (\)&3\); n>0; n--\) {)__
popA exec
NL (})__
} def

|-------------------------------------------- DYencode function:
| constructs C code to convert a double value into destination type
| type and to return the converted value.

/DYencode {
NL (static void D)__ dt__ (encode\(D t, B *dp\))__
NL ({)__
dt { (dp)__ } (t) CVPUT
NL (})__
} def

|-------------------------------------------- XLvalue function:
| constructs C code to convert a double value into any type
| type and to return the converted value.

/XLvalue {

NL (static L )__ st__ (Lvalue\(B *sp\))__
NL ({)__
NL (D t; L tc;)__
st { (sp)__ } (t) GETCV
/L { (&tc)__ } (t) CVPUT
NL (return\(tc\);)__
NL (})__
} def

|-------------------------------------------- XDtest function:
| constructs C code to convert a source type value into double
| type and to return the double value.

/XDtest {

NL (static D )__ st__ (Dtest\(B *sp\))__
NL ({)__
NL (D t;)__
st { (sp)__ } (t) GETCV
NL (return\(t\);)__
NL (})__
} def

|-------------------------------------------- XYmoveSS function:
| constructs C code to move a scalar from one frame to another, con-
| verting the numeral type as necessary and preserving an old or, on
| overflow, creating a new undefined value. 

/XYmoveSS {

NL (static void )__ st__ dt__ (moveSS\(B *sf, B *df\))__
NL ({)__
NL (D t;)__
st { (sf) SCALAR } (t) GETCV
dt { (df) SCALAR } (t) CVPUT
NL (})__
NL 
} def

|-------------------------------------------- XYmoveSA function:
| constructs C code to spread a scalar into an array (see XYmoveSS)

/XYmoveSA {

NL (static void )__ st__ dt__ (moveSA\(B *sf, B *df\))__
NL ({)__
NL (D t; L n; )__ dt__ ( *d;)__
NL (d = \()__ dt__ ( *\))__ (df) ARRAY (;)__
st { (sf) SCALAR } (t) GETCV
(df) { dt { (d++)__ } (t) CVPUT } FOR
NL (})__
NL 
} def

|-------------------------------------------- XYmoveAS function:
| constructs C code to move one array value cell into a scalar

/XYmoveAS {

NL (static void )__ st__ dt__ (moveAS\(B *sf, B *df\))__
NL ({)__
NL (D t; )__ st__ ( *s; )__
NL (s = \()__ st__ ( *\))__ (sf) ARRAY (;)__
NL st { (s)__ } (t) GETCV
NL dt { (df) SCALAR } (t) CVPUT
NL (})__
NL
} def

|-------------------------------------------- XYmoveAA function:
| constructs C code to copy an array into an array (see XYmoveSS)

/XYmoveAA {

NL (static void )__ st__ dt__ (moveAA\(B *sf, B *df\))__
NL ({)__
NL (D t; L n; )__ st__ ( *s; )__ dt__ ( *d;)__
NL (s = \()__ st__ ( *\))__ (sf) ARRAY (;)__
NL (d = \()__ dt__ ( *\))__ (df) ARRAY (;)__
(df) { st { (s++)__ } (t) GETCV
       dt { (d++)__ } (t) CVPUT
     } FOR
NL (})__
NL 
} def

|-------------------------------------------- XYdyOPSS function:
| constructs C code to convert the values of two scalar frames (source
| and destination) of any types into double type, to perform a dyadic
| operation on them, to convert the result into the destination type
| and to store the result.

/XYdyOPSS {

NL (static void )__ dt__ st__ (dy)__ op__ (SS\(B *df, B *sf\))__
NL ({)__
NL (D t, tt;)__
dt { (df) SCALAR } (t) GETCV
st { (sf) SCALAR } (tt) GETCV
dyadic_d op get exec
dt { (df) SCALAR } (t) CVPUT
NL (})__
NL 
} def

/dyadic_d 7 dict dup begin
/ADD { NL (t += tt;)__ } def
/SUB { NL (t -= tt;)__ } def
/MUL { NL (t *= tt;)__ } def
/DIV { NL (t /= tt;)__ } def
/PWR { NL (t = pow\(t,tt\);)__ } def
/MOD { NL (t = fmod\(t,tt\);)__ } def
/THEARC {NL (t = thearc\(t,tt\);)__} def
end def

/dyadic_d_x 7 dict dup begin
/ADD { pushA NL (t)__ popA dup pushA __ ( += tt)__ popA __ (;)__ } def
/SUB { pushA NL (t)__ popA dup pushA __ ( -= tt)__ popA __ (;)__ } def
/MUL { pushA NL (t)__ popA dup pushA __ ( *= tt)__ popA __ (;)__ } def
/DIV { pushA NL (t)__ popA dup pushA __ ( /= tt)__ popA __ (;)__ } def
/PWR { pushA NL (t)__ popA dup pushA __ ( = pow\(t)__ popA dup pushA __ 
       (,tt)__ popA __ (\);)__ } def
/MOD { pushA NL (t)__ popA dup pushA __ ( = fmod\(t)__ popA dup pushA __ 
       (,tt)__ popA __ (\);)__ } def
/THEARC { pushA NL (t)__ popA dup pushA __ ( = thearc\(t)__ popA dup pushA __
       (,tt)__ popA __ (\);)__ } def
end def

|-------------------------------------------- XYdyOPAS function:
| constructs C code to convert the value of a scalar source frame
| and of all elements of the value of an array destination frame, which
| can be of any types, into double type,  to perform a dyadic
| operation on them, to convert the results into the destination type
| and to store the results replacing the original elements of the
| destination array.

/XYdyOPAS {

NL (static void )__ dt__ st__ (dy)__ op__ (AS\(B *df, B *sf\))__
NL ({)__
NL (D t,tt; L n; )__ dt__ ( *d;)__
NL (d = \()__ dt__ ( *\))__ (df) ARRAY (;)__
st { (sf) SCALAR } (tt) GETCV
(df) { dt { (d)__ } (t) GETCV
       dyadic_d op get exec
       dt { (d++)__ } (t) CVPUT
     } FOR
NL (})__
NL 
} def

|-------------------------------------------- XYdyOPSA function:
| constructs C code to convert the value of a destination scalar frame
| and of all elements of the value of a array source frame, which
| can be of any types, into double type,  to perform a dyadic
| operation involving each source element and a running result (primed
| to the destination value), to convert the accumulated result into the
| destination type and to store it in the destination scalar.

/XYdyOPSA {

NL (static void )__ dt__ st__ (dy)__ op__ (SA\(B *df, B *sf\))__
NL ({)__
NL (D t,tt; L n; )__ st__ ( *s;)__
NL (s = \()__ st__ ( *\))__ (sf) ARRAY (;)__
dt { (df) SCALAR } (t) GETCV
(sf) { st { (s++)__ } (tt) GETCV
       dyadic_d op get exec
     } FOR
dt { (df) SCALAR } (t) CVPUT
NL (})__
NL 
} def

|-------------------------------------------- XYdyOPAA function:
| constructs C code to convert the values of two array frames (source
| and destination), which can be of any types, into double type,  to
| perform a dyadic operation on them, to convert the results into the
| destination type and to store the results replacing the original
| elements of the destination array.
|
| In this improved version, the loop is rolled out (modulo 4 for the time).
|

/XYdyOPAA {

NL (static void )__ dt__ st__ (dy)__ op__ (AA\(B *df, B *sf\))__
NL ({)__
NL (register L n; register )__  st__ ( *s; register )__ dt__ ( *d;)__
NL (register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; )__ 
NL (s = \()__ st__ ( *\))__ (sf) ARRAY (;)__
NL (d = \()__ dt__ ( *\))__ (df) ARRAY (;)__

 (df) { st { (s++)__ } (tt1) GETCV
        dt { (d)__ } (t1) GETCV
        (1) dyadic_d_x op get exec
        st { (s++)__ } (tt2) GETCV
        dt { (\(d+1\))__ } (t2) GETCV
        (2) dyadic_d_x op get exec
        st { (s++)__ } (tt3) GETCV
        dt { (\(d+2\))__ } (t3) GETCV
        (3) dyadic_d_x op get exec
        st { (s++)__ } (tt4) GETCV
        dt { (\(d+3\))__ } (t4) GETCV
        (4) dyadic_d_x op get exec
        dt { (d++)__ } (t1) CVPUT
        dt { (d++)__ } (t2) CVPUT
        dt { (d++)__ } (t3) CVPUT
        dt { (d++)__ } (t4) CVPUT
      } FOR_mod4
 (df) { st { (s++)__ } (tt1) GETCV
        dt { (d)__ } (t1) GETCV
        (1) dyadic_d_x op get exec
        dt { (d++)__ } (t1) CVPUT
      } FOR_mod1
NL (})__
NL 
} def

|-------------------------------------------- YmoOPS function:
| constructs C code to convert the value of a scalar frame of any type
| into double type, to perform a monadic operation on it, to convert the
| result into the destination type and to store the result.

/YmoOPS {

NL (static void )__ dt__ (mo)__ op__ (S\(B *df\))__
NL ({)__
NL (D t;)__
dt { (df) SCALAR } (t) GETCV
monadic_d op get exec
dt { (df) SCALAR } (t) CVPUT
NL (})__
NL 
} def

/monadic_d 14 dict dup begin

/NEG   { NL (t = -t;)__          } def
/ABS   { NL (t = fabs\(t\);)__    } def
/SQRT  { NL (t = sqrt\(t\);)__   } def
/EXP   { NL (t = exp\(t\);)__    } def
/LN    { NL (t = log\(t\);)__    } def
/LG    { NL (t = log10\(t\);)__  } def
/FLOOR { NL (t = floor\(t\);)__  } def
/CEIL  { NL (t = ceil\(t\);)__   } def
/SIN   { NL (t = sin\(t\);)__    } def
/COS   { NL (t = cos\(t\);)__    } def
/TAN   { NL (t = tan\(t\);)__    } def
/ASIN  { NL (t = asin\(t\);)__   } def
/ACOS  { NL (t = acos\(t\);)__   } def
/ATAN  { NL (t = atan\(t\);)__   } def

end def

|-------------------------------------------- YmoOPA function:
| constructs C code to convert the value of an array frame of any type
| into double type, to perform a monadic operation on it, to convert the
| result into the destination type and to store the result, overwriting
| the original.

/YmoOPA {

NL (static void )__ dt__ (mo)__ op__ (A\(B *df\))__
NL ({)__
NL (D t; )__ dt__ ( *d; L n;)__
NL (d = \()__ dt__ ( *\))__ (df) ARRAY (;)__
(df) { dt { (d)__ } (t) GETCV
       monadic_d op get exec
       dt { (d++)__ } (t) CVPUT
     } FOR
NL (})__
NL 
} def

|-------------------------------------------- Ydecr function
| constructs C code to convert the value of a scalar frame of any type
| into double type, to subtract 1 from that value, to convert the
| result into the frame type and to store the result.

/Ydecr {

NL (static void )__ dt__ (decr\(B *df\))__
NL ({)__
NL (D t;)__
dt { (df) SCALAR } (t) GETCV
NL (t -= 1.0;)__
dt { (df) SCALAR } (t) CVPUT
NL (})__
NL 
} def

|-------------------- construct DMNUMINC.C -----------------------------
| (dirname) | --

/all {  /dirname name
  {

NL

|----- generate ENCODE function definitions
[ /B /W /L  /S /D ] { /dt name DYencode } forall

|----- generate definition of list of ENCODE functions

NL (typedef void \(*ENCODEfct\)\(D,B*\);)__
NL (static ENCODEfct ENCODElist[] = {)__
[ /B /W /L  /S /D ]
   {  /dt name 
      NL (D)__ dt__ (encode, )__
   } forall
NL(};)__
NL

|----- generate VALUE function definitions
[ /B /W /L  /S /D ] { /st name XLvalue } forall

|----- generate definition of list of VALUE functions

NL (typedef L \(*VALUEfct\)\(B*\);)__
NL (static VALUEfct VALUElist[] = {)__
[ /B /W /L  /S /D ]
   {  /st name 
      NL st__ (Lvalue, )__
   } forall
NL(};)__
NL

|----- generate TEST function definitions
[ /B /W /L  /S /D ] { /st name XDtest } forall

|----- generate definition of list of TEST functions

NL (typedef D \(*TESTfct\)\(B*\);)__
NL (static TESTfct TESTlist[] = {)__
[ /B /W /L  /S /D ]
   {  /st name 
      NL st__ (Dtest, )__
   } forall
NL(};)__
NL

|----- generate MOVE function definitions
[ /B /W /L  /S /D ]
{  /st name 
   [ /B /W /L  /S /D ]
   {  /dt name 
      [ /XYmoveSS /XYmoveSA /XYmoveAS /XYmoveAA ] { mkact exec } forall
   } forall
} forall

|----- generate definition of list of MOVE functions

NL (typedef void \(*MOVEfct\)\(B*,B*\);)__
NL (static MOVEfct MOVElist[] = {)__
NL
[ /B /W /L  /S /D ]
{  /st name 
   [ /B /W /L  /S /D ]
   {  /dt name 
      [ /SS /SA /AS /AA ]
      { /cc name st__ dt__ (move)__ cc mkact __ (, )__ } forall
      NL
   } forall
} forall
NL(};)__
NL

|----- generate dyadic operator function definitions
[ /B /W /L  /S /D ]
{  /dt name 
   [ /B /W /L  /S /D ]
   {  /st name
      [ /ADD /SUB /MUL /DIV /PWR /MOD /THEARC ]
      { /op name 
        [ /XYdyOPSS /XYdyOPAS /XYdyOPSA /XYdyOPAA ] { mkact exec } forall
      } forall
   } forall
} forall

|----- generate definition of list of dyadic operator functions

NL (typedef void \(*dyadic_fct\)\(B*,B*\);)__

[ /ADD /SUB /MUL /DIV /PWR /MOD /THEARC ] { /op name
NL (static dyadic_fct )__ op__ (list[] = {)__
NL
[ /B /W /L  /S /D ]
{  /dt name 
   [ /B /W /L  /S /D ]
   { /st name 
     [ /SS /AS /SA /AA ] 
     { /cc name
       dt__ st__ (dy)__ op mkact __ cc mkact __ mkact exec (, )__
     } forall
        NL
   } forall
} forall
NL(};)__
} forall
NL

|----- generate monadic operator function definitions
[ /B /W /L  /S /D ]
{  /dt name 
  [ /NEG /ABS /SQRT /EXP /LN /LG /FLOOR /CEIL /SIN /COS /TAN
    /ASIN /ACOS /ATAN
  ]
  { /op name 
    [ /YmoOPS /YmoOPA ] { mkact exec } forall
  } forall
} forall

|----- generate definitions of monadic operator lists

NL (typedef void \(*monadic_fct\)\(B*\);)__

[ /NEG /ABS /SQRT /EXP /LN /LG /FLOOR /CEIL /SIN /COS /TAN
    /ASIN /ACOS /ATAN ]
{ /op name
NL (static monadic_fct )__ op__ (list[] = {)__
NL
[ /B /W /L  /S /D ]
{ /dt name 
  [ /S /A ] 
  { /cc name
    dt__ (mo)__ op mkact __ cc mkact __ mkact exec (, )__
  } forall
  NL
} forall
NL(};)__
} forall

|----- generate DECREMENT function definitions

[ /B /W /L  /S /D ] { /dt name Ydecr } forall

|----- generate definitions of DECREMENT function lists

NL (typedef void \(*DECR_fct\)\(B*\);)__

NL (static DECR_fct DECRlist[] = {)__
NL
[ /B /W /L  /S /D ]
{ /dt name dt__ (decr, )__ } forall
NL(};)__
NL
    
|------------ save the file
}
save /dmsave name /dmfile 400000 /b array def dmsave capsave 
dmfile 0 3 -1 roll exec
0 exch getinterval dirname (dmnuminc.c) writefile
dmsave restore 
} def

|------------------------ Testing constructors ------------------------

/moveSS { /dt name /st name
AMbase 0 XYmoveSS 0 exch getinterval /Rss name Rss toconsole
} def

/moveSA { /dt name /st name
AMbase 0 XYmoveSA 0 exch getinterval /Rss name Rss toconsole
} def

/moveAS { /dt name /st name
AMbase 0 XYmoveAS 0 exch getinterval /Rss name Rss toconsole
} def

/moveAA { /dt name /st name
AMbase 0 XYmoveAA 0 exch getinterval /Rss name Rss toconsole
} def

/encode { /dt name
AMbase 0 DYencode 0 exch getinterval /Rss name Rss toconsole 
} def

/value { /st name
AMbase 0 XLvalue 0 exch getinterval /Rss name Rss toconsole 
} def

/test { /st name
AMbase 0 XDtest 0 exch getinterval /Rss name Rss toconsole 
} def

/dySS { /op name /st name /dt name 
AMbase 0 XYdyOPSS 0 exch getinterval /Rss name Rss toconsole
} def

/dyAS { /op name /st name /dt name 
AMbase 0 XYdyOPAS 0 exch getinterval /Rss name Rss toconsole
} def

/dySA { /op name /st name /dt name 
AMbase 0 XYdyOPSA 0 exch getinterval /Rss name Rss toconsole
} def

/dyAA { /op name /st name /dt name 
AMbase 0 XYdyOPAA 0 exch getinterval /Rss name Rss toconsole
} def

/moS { /op name /dt name
AMbase 0 YmoOPS 0 exch getinterval /Rss name Rss toconsole
} def

/moA { /op name /dt name
AMbase 0 YmoOPA 0 exch getinterval /Rss name Rss toconsole
} def

end userdict /dmnuminc put

