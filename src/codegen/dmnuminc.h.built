

static void DBencode(D t, B *dp)
{
*((B *)dp) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
static void DWencode(D t, B *dp)
{
*((W *)dp) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
static void DLencode(D t, B *dp)
{
*((L *)dp) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
static void DSencode(D t, B *dp)
{
*((S *)dp) = t;
}
static void DDencode(D t, B *dp)
{
*((D *)dp) = t;
}
typedef void (*ENCODEfct)(D,B*);
static ENCODEfct ENCODElist[] = {
DBencode, 
DWencode, 
DLencode, 
DSencode, 
DDencode, 
};

static L BLvalue(B *sp)
{
D t; L tc;
if ((t = *((B *)sp)) == BINF) t = DINF;
*((L *)&tc) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
return(tc);
}
static L WLvalue(B *sp)
{
D t; L tc;
if ((t = *((W *)sp)) == WINF) t = DINF;
*((L *)&tc) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
return(tc);
}
static L LLvalue(B *sp)
{
D t; L tc;
if ((t = *((L *)sp)) == LINF) t = DINF;
*((L *)&tc) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
return(tc);
}
static L SLvalue(B *sp)
{
D t; L tc;
t = *((S *)sp);
*((L *)&tc) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
return(tc);
}
static L DLvalue(B *sp)
{
D t; L tc;
t = *((D *)sp);
*((L *)&tc) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
return(tc);
}
typedef L (*VALUEfct)(B*);
static VALUEfct VALUElist[] = {
BLvalue, 
WLvalue, 
LLvalue, 
SLvalue, 
DLvalue, 
};

static D BDtest(B *sp)
{
D t;
if ((t = *((B *)sp)) == BINF) t = DINF;
return(t);
}
static D WDtest(B *sp)
{
D t;
if ((t = *((W *)sp)) == WINF) t = DINF;
return(t);
}
static D LDtest(B *sp)
{
D t;
if ((t = *((L *)sp)) == LINF) t = DINF;
return(t);
}
static D SDtest(B *sp)
{
D t;
t = *((S *)sp);
return(t);
}
static D DDtest(B *sp)
{
D t;
t = *((D *)sp);
return(t);
}
typedef D (*TESTfct)(B*);
static TESTfct TESTlist[] = {
BDtest, 
WDtest, 
LDtest, 
SDtest, 
DDtest, 
};

static void BBmoveSS(B *sf, B *df)
{
D t;
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBmoveSA(B *sf, B *df)
{
D t; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBmoveAS(B *sf, B *df)
{
D t; B *s; 
s = (B *)VALUE_BASE(sf);

if ((t = *((B *)s)) == BINF) t = DINF;

*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBmoveAA(B *sf, B *df)
{
D t; L n; B *s; B *d;
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)s++)) == BINF) t = DINF;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWmoveSS(B *sf, B *df)
{
D t;
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void BWmoveSA(B *sf, B *df)
{
D t; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void BWmoveAS(B *sf, B *df)
{
D t; B *s; 
s = (B *)VALUE_BASE(sf);

if ((t = *((B *)s)) == BINF) t = DINF;

*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void BWmoveAA(B *sf, B *df)
{
D t; L n; B *s; W *d;
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)s++)) == BINF) t = DINF;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void BLmoveSS(B *sf, B *df)
{
D t;
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void BLmoveSA(B *sf, B *df)
{
D t; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void BLmoveAS(B *sf, B *df)
{
D t; B *s; 
s = (B *)VALUE_BASE(sf);

if ((t = *((B *)s)) == BINF) t = DINF;

*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void BLmoveAA(B *sf, B *df)
{
D t; L n; B *s; L *d;
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)s++)) == BINF) t = DINF;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void BSmoveSS(B *sf, B *df)
{
D t;
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
*((S *)NUM_VAL(df)) = t;
}

static void BSmoveSA(B *sf, B *df)
{
D t; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((S *)d++) = t;
}
}

static void BSmoveAS(B *sf, B *df)
{
D t; B *s; 
s = (B *)VALUE_BASE(sf);

if ((t = *((B *)s)) == BINF) t = DINF;

*((S *)NUM_VAL(df)) = t;
}

static void BSmoveAA(B *sf, B *df)
{
D t; L n; B *s; S *d;
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)s++)) == BINF) t = DINF;
*((S *)d++) = t;
}
}

static void BDmoveSS(B *sf, B *df)
{
D t;
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
*((D *)NUM_VAL(df)) = t;
}

static void BDmoveSA(B *sf, B *df)
{
D t; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((t = *((B *)NUM_VAL(sf))) == BINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((D *)d++) = t;
}
}

static void BDmoveAS(B *sf, B *df)
{
D t; B *s; 
s = (B *)VALUE_BASE(sf);

if ((t = *((B *)s)) == BINF) t = DINF;

*((D *)NUM_VAL(df)) = t;
}

static void BDmoveAA(B *sf, B *df)
{
D t; L n; B *s; D *d;
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)s++)) == BINF) t = DINF;
*((D *)d++) = t;
}
}

static void WBmoveSS(B *sf, B *df)
{
D t;
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void WBmoveSA(B *sf, B *df)
{
D t; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void WBmoveAS(B *sf, B *df)
{
D t; W *s; 
s = (W *)VALUE_BASE(sf);

if ((t = *((W *)s)) == WINF) t = DINF;

*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void WBmoveAA(B *sf, B *df)
{
D t; L n; W *s; B *d;
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)s++)) == WINF) t = DINF;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void WWmoveSS(B *sf, B *df)
{
D t;
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWmoveSA(B *sf, B *df)
{
D t; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWmoveAS(B *sf, B *df)
{
D t; W *s; 
s = (W *)VALUE_BASE(sf);

if ((t = *((W *)s)) == WINF) t = DINF;

*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWmoveAA(B *sf, B *df)
{
D t; L n; W *s; W *d;
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)s++)) == WINF) t = DINF;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLmoveSS(B *sf, B *df)
{
D t;
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void WLmoveSA(B *sf, B *df)
{
D t; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void WLmoveAS(B *sf, B *df)
{
D t; W *s; 
s = (W *)VALUE_BASE(sf);

if ((t = *((W *)s)) == WINF) t = DINF;

*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void WLmoveAA(B *sf, B *df)
{
D t; L n; W *s; L *d;
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)s++)) == WINF) t = DINF;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void WSmoveSS(B *sf, B *df)
{
D t;
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
*((S *)NUM_VAL(df)) = t;
}

static void WSmoveSA(B *sf, B *df)
{
D t; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((S *)d++) = t;
}
}

static void WSmoveAS(B *sf, B *df)
{
D t; W *s; 
s = (W *)VALUE_BASE(sf);

if ((t = *((W *)s)) == WINF) t = DINF;

*((S *)NUM_VAL(df)) = t;
}

static void WSmoveAA(B *sf, B *df)
{
D t; L n; W *s; S *d;
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)s++)) == WINF) t = DINF;
*((S *)d++) = t;
}
}

static void WDmoveSS(B *sf, B *df)
{
D t;
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
*((D *)NUM_VAL(df)) = t;
}

static void WDmoveSA(B *sf, B *df)
{
D t; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((t = *((W *)NUM_VAL(sf))) == WINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((D *)d++) = t;
}
}

static void WDmoveAS(B *sf, B *df)
{
D t; W *s; 
s = (W *)VALUE_BASE(sf);

if ((t = *((W *)s)) == WINF) t = DINF;

*((D *)NUM_VAL(df)) = t;
}

static void WDmoveAA(B *sf, B *df)
{
D t; L n; W *s; D *d;
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)s++)) == WINF) t = DINF;
*((D *)d++) = t;
}
}

static void LBmoveSS(B *sf, B *df)
{
D t;
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void LBmoveSA(B *sf, B *df)
{
D t; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void LBmoveAS(B *sf, B *df)
{
D t; L *s; 
s = (L *)VALUE_BASE(sf);

if ((t = *((L *)s)) == LINF) t = DINF;

*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void LBmoveAA(B *sf, B *df)
{
D t; L n; L *s; B *d;
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)s++)) == LINF) t = DINF;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void LWmoveSS(B *sf, B *df)
{
D t;
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void LWmoveSA(B *sf, B *df)
{
D t; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void LWmoveAS(B *sf, B *df)
{
D t; L *s; 
s = (L *)VALUE_BASE(sf);

if ((t = *((L *)s)) == LINF) t = DINF;

*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void LWmoveAA(B *sf, B *df)
{
D t; L n; L *s; W *d;
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)s++)) == LINF) t = DINF;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void LLmoveSS(B *sf, B *df)
{
D t;
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLmoveSA(B *sf, B *df)
{
D t; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLmoveAS(B *sf, B *df)
{
D t; L *s; 
s = (L *)VALUE_BASE(sf);

if ((t = *((L *)s)) == LINF) t = DINF;

*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLmoveAA(B *sf, B *df)
{
D t; L n; L *s; L *d;
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)s++)) == LINF) t = DINF;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSmoveSS(B *sf, B *df)
{
D t;
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
*((S *)NUM_VAL(df)) = t;
}

static void LSmoveSA(B *sf, B *df)
{
D t; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((S *)d++) = t;
}
}

static void LSmoveAS(B *sf, B *df)
{
D t; L *s; 
s = (L *)VALUE_BASE(sf);

if ((t = *((L *)s)) == LINF) t = DINF;

*((S *)NUM_VAL(df)) = t;
}

static void LSmoveAA(B *sf, B *df)
{
D t; L n; L *s; S *d;
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)s++)) == LINF) t = DINF;
*((S *)d++) = t;
}
}

static void LDmoveSS(B *sf, B *df)
{
D t;
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
*((D *)NUM_VAL(df)) = t;
}

static void LDmoveSA(B *sf, B *df)
{
D t; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((t = *((L *)NUM_VAL(sf))) == LINF) t = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
*((D *)d++) = t;
}
}

static void LDmoveAS(B *sf, B *df)
{
D t; L *s; 
s = (L *)VALUE_BASE(sf);

if ((t = *((L *)s)) == LINF) t = DINF;

*((D *)NUM_VAL(df)) = t;
}

static void LDmoveAA(B *sf, B *df)
{
D t; L n; L *s; D *d;
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)s++)) == LINF) t = DINF;
*((D *)d++) = t;
}
}

static void SBmoveSS(B *sf, B *df)
{
D t;
t = *((S *)NUM_VAL(sf));
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void SBmoveSA(B *sf, B *df)
{
D t; L n; B *d;
d = (B *)VALUE_BASE(df);
t = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void SBmoveAS(B *sf, B *df)
{
D t; S *s; 
s = (S *)VALUE_BASE(sf);

t = *((S *)s);

*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void SBmoveAA(B *sf, B *df)
{
D t; L n; S *s; B *d;
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)s++);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void SWmoveSS(B *sf, B *df)
{
D t;
t = *((S *)NUM_VAL(sf));
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void SWmoveSA(B *sf, B *df)
{
D t; L n; W *d;
d = (W *)VALUE_BASE(df);
t = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void SWmoveAS(B *sf, B *df)
{
D t; S *s; 
s = (S *)VALUE_BASE(sf);

t = *((S *)s);

*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void SWmoveAA(B *sf, B *df)
{
D t; L n; S *s; W *d;
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)s++);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void SLmoveSS(B *sf, B *df)
{
D t;
t = *((S *)NUM_VAL(sf));
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void SLmoveSA(B *sf, B *df)
{
D t; L n; L *d;
d = (L *)VALUE_BASE(df);
t = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void SLmoveAS(B *sf, B *df)
{
D t; S *s; 
s = (S *)VALUE_BASE(sf);

t = *((S *)s);

*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void SLmoveAA(B *sf, B *df)
{
D t; L n; S *s; L *d;
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)s++);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void SSmoveSS(B *sf, B *df)
{
D t;
t = *((S *)NUM_VAL(sf));
*((S *)NUM_VAL(df)) = t;
}

static void SSmoveSA(B *sf, B *df)
{
D t; L n; S *d;
d = (S *)VALUE_BASE(df);
t = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((S *)d++) = t;
}
}

static void SSmoveAS(B *sf, B *df)
{
D t; S *s; 
s = (S *)VALUE_BASE(sf);

t = *((S *)s);

*((S *)NUM_VAL(df)) = t;
}

static void SSmoveAA(B *sf, B *df)
{
D t; L n; S *s; S *d;
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)s++);
*((S *)d++) = t;
}
}

static void SDmoveSS(B *sf, B *df)
{
D t;
t = *((S *)NUM_VAL(sf));
*((D *)NUM_VAL(df)) = t;
}

static void SDmoveSA(B *sf, B *df)
{
D t; L n; D *d;
d = (D *)VALUE_BASE(df);
t = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((D *)d++) = t;
}
}

static void SDmoveAS(B *sf, B *df)
{
D t; S *s; 
s = (S *)VALUE_BASE(sf);

t = *((S *)s);

*((D *)NUM_VAL(df)) = t;
}

static void SDmoveAA(B *sf, B *df)
{
D t; L n; S *s; D *d;
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)s++);
*((D *)d++) = t;
}
}

static void DBmoveSS(B *sf, B *df)
{
D t;
t = *((D *)NUM_VAL(sf));
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void DBmoveSA(B *sf, B *df)
{
D t; L n; B *d;
d = (B *)VALUE_BASE(df);
t = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void DBmoveAS(B *sf, B *df)
{
D t; D *s; 
s = (D *)VALUE_BASE(sf);

t = *((D *)s);

*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void DBmoveAA(B *sf, B *df)
{
D t; L n; D *s; B *d;
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)s++);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void DWmoveSS(B *sf, B *df)
{
D t;
t = *((D *)NUM_VAL(sf));
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void DWmoveSA(B *sf, B *df)
{
D t; L n; W *d;
d = (W *)VALUE_BASE(df);
t = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void DWmoveAS(B *sf, B *df)
{
D t; D *s; 
s = (D *)VALUE_BASE(sf);

t = *((D *)s);

*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void DWmoveAA(B *sf, B *df)
{
D t; L n; D *s; W *d;
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)s++);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void DLmoveSS(B *sf, B *df)
{
D t;
t = *((D *)NUM_VAL(sf));
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void DLmoveSA(B *sf, B *df)
{
D t; L n; L *d;
d = (L *)VALUE_BASE(df);
t = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void DLmoveAS(B *sf, B *df)
{
D t; D *s; 
s = (D *)VALUE_BASE(sf);

t = *((D *)s);

*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void DLmoveAA(B *sf, B *df)
{
D t; L n; D *s; L *d;
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)s++);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void DSmoveSS(B *sf, B *df)
{
D t;
t = *((D *)NUM_VAL(sf));
*((S *)NUM_VAL(df)) = t;
}

static void DSmoveSA(B *sf, B *df)
{
D t; L n; S *d;
d = (S *)VALUE_BASE(df);
t = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((S *)d++) = t;
}
}

static void DSmoveAS(B *sf, B *df)
{
D t; D *s; 
s = (D *)VALUE_BASE(sf);

t = *((D *)s);

*((S *)NUM_VAL(df)) = t;
}

static void DSmoveAA(B *sf, B *df)
{
D t; L n; D *s; S *d;
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)s++);
*((S *)d++) = t;
}
}

static void DDmoveSS(B *sf, B *df)
{
D t;
t = *((D *)NUM_VAL(sf));
*((D *)NUM_VAL(df)) = t;
}

static void DDmoveSA(B *sf, B *df)
{
D t; L n; D *d;
d = (D *)VALUE_BASE(df);
t = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
*((D *)d++) = t;
}
}

static void DDmoveAS(B *sf, B *df)
{
D t; D *s; 
s = (D *)VALUE_BASE(sf);

t = *((D *)s);

*((D *)NUM_VAL(df)) = t;
}

static void DDmoveAA(B *sf, B *df)
{
D t; L n; D *s; D *d;
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)s++);
*((D *)d++) = t;
}
}

typedef void (*MOVEfct)(B*,B*);
static MOVEfct MOVElist[] = {
BBmoveSS, BBmoveSA, BBmoveAS, BBmoveAA, 
BWmoveSS, BWmoveSA, BWmoveAS, BWmoveAA, 
BLmoveSS, BLmoveSA, BLmoveAS, BLmoveAA, 
BSmoveSS, BSmoveSA, BSmoveAS, BSmoveAA, 
BDmoveSS, BDmoveSA, BDmoveAS, BDmoveAA, 
WBmoveSS, WBmoveSA, WBmoveAS, WBmoveAA, 
WWmoveSS, WWmoveSA, WWmoveAS, WWmoveAA, 
WLmoveSS, WLmoveSA, WLmoveAS, WLmoveAA, 
WSmoveSS, WSmoveSA, WSmoveAS, WSmoveAA, 
WDmoveSS, WDmoveSA, WDmoveAS, WDmoveAA, 
LBmoveSS, LBmoveSA, LBmoveAS, LBmoveAA, 
LWmoveSS, LWmoveSA, LWmoveAS, LWmoveAA, 
LLmoveSS, LLmoveSA, LLmoveAS, LLmoveAA, 
LSmoveSS, LSmoveSA, LSmoveAS, LSmoveAA, 
LDmoveSS, LDmoveSA, LDmoveAS, LDmoveAA, 
SBmoveSS, SBmoveSA, SBmoveAS, SBmoveAA, 
SWmoveSS, SWmoveSA, SWmoveAS, SWmoveAA, 
SLmoveSS, SLmoveSA, SLmoveAS, SLmoveAA, 
SSmoveSS, SSmoveSA, SSmoveAS, SSmoveAA, 
SDmoveSS, SDmoveSA, SDmoveAS, SDmoveAA, 
DBmoveSS, DBmoveSA, DBmoveAS, DBmoveAA, 
DWmoveSS, DWmoveSA, DWmoveAS, DWmoveAA, 
DLmoveSS, DLmoveSA, DLmoveAS, DLmoveAA, 
DSmoveSS, DSmoveSA, DSmoveAS, DSmoveAA, 
DDmoveSS, DDmoveSA, DDmoveAS, DDmoveAA, 

};

static void BBdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t += tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyADDAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t += tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyADDSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t += tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyADDAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 += tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t -= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdySUBAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t -= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdySUBSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t -= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdySUBAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 -= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t *= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyMULAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t *= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyMULSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t *= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyMULAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 *= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t /= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyDIVAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t /= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyDIVSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t /= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyDIVAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 /= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = pow(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyPWRAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = pow(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyPWRSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = pow(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyPWRAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = pow(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = fmod(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyMODAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fmod(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyMODSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = fmod(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyMODAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BBdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = thearc(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = thearc(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BBdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = thearc(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BBdyTHEARCAA(B *df, B *sf)
{
register L n; register B *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t += tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyADDAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t += tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyADDSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t += tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyADDAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 += tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t -= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdySUBAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t -= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdySUBSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t -= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdySUBAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 -= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t *= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyMULAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t *= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyMULSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t *= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyMULAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 *= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t /= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyDIVAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t /= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyDIVSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t /= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyDIVAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 /= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = pow(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyPWRAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = pow(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyPWRSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = pow(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyPWRAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = pow(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = fmod(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyMODAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fmod(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyMODSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = fmod(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyMODAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BWdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = thearc(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = thearc(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BWdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = thearc(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BWdyTHEARCAA(B *df, B *sf)
{
register L n; register W *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t += tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyADDAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t += tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyADDSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t += tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyADDAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 += tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t -= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdySUBAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t -= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdySUBSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t -= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdySUBAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 -= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t *= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyMULAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t *= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyMULSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t *= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyMULAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 *= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t /= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyDIVAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t /= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyDIVSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t /= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyDIVAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 /= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = pow(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyPWRAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = pow(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyPWRSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = pow(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyPWRAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = pow(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = fmod(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyMODAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fmod(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyMODSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = fmod(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyMODAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BLdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = thearc(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = thearc(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BLdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = thearc(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BLdyTHEARCAA(B *df, B *sf)
{
register L n; register L *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t += tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyADDAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t += tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyADDSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t += tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyADDAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 += tt2;
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 += tt3;
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 += tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t -= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdySUBAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t -= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdySUBSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t -= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdySUBAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 -= tt2;
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 -= tt3;
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 -= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t *= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyMULAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t *= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyMULSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t *= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyMULAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 *= tt2;
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 *= tt3;
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 *= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t /= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyDIVAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t /= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyDIVSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t /= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyDIVAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 /= tt2;
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 /= tt3;
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 /= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = pow(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyPWRAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = pow(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyPWRSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = pow(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyPWRAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = pow(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = fmod(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyMODAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fmod(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyMODSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = fmod(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyMODAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BSdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = thearc(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = thearc(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BSdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = thearc(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BSdyTHEARCAA(B *df, B *sf)
{
register L n; register S *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t += tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyADDAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t += tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyADDSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t += tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyADDAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 += tt2;
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 += tt3;
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 += tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 += tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t -= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdySUBAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t -= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdySUBSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t -= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdySUBAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 -= tt2;
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 -= tt3;
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 -= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 -= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t *= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyMULAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t *= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyMULSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t *= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyMULAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 *= tt2;
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 *= tt3;
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 *= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 *= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t /= tt;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyDIVAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t /= tt;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyDIVSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t /= tt;
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyDIVAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 /= tt2;
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 /= tt3;
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 /= tt4;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 /= tt1;
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = pow(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyPWRAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = pow(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyPWRSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = pow(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyPWRAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = pow(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = pow(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = fmod(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyMODAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fmod(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyMODSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = fmod(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyMODAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void BDdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = thearc(t,tt);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; B *d;
d = (B *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = thearc(t,tt);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BDdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = thearc(t,tt);
}
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BDdyTHEARCAA(B *df, B *sf)
{
register L n; register D *s; register B *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (B *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((B *)(d+1))) == BINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((B *)(d+2))) == BINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((B *)(d+3))) == BINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
*((B *)d++) = (((t2) > BMAX) || ((-t2) < -BMAX) || (t2 == DINF))? BINF : t2;
*((B *)d++) = (((t3) > BMAX) || ((-t3) < -BMAX) || (t3 == DINF))? BINF : t3;
*((B *)d++) = (((t4) > BMAX) || ((-t4) < -BMAX) || (t4 == DINF))? BINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((B *)d)) == BINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((B *)d++) = (((t1) > BMAX) || ((-t1) < -BMAX) || (t1 == DINF))? BINF : t1;
}
}

static void WBdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t += tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyADDAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t += tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyADDSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t += tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyADDAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 += tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t -= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdySUBAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t -= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdySUBSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t -= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdySUBAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 -= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t *= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyMULAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t *= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyMULSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t *= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyMULAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 *= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t /= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyDIVAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t /= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyDIVSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t /= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyDIVAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 /= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = pow(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyPWRAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = pow(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyPWRSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = pow(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyPWRAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = pow(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = fmod(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyMODAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fmod(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyMODSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = fmod(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyMODAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WBdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = thearc(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = thearc(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WBdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = thearc(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WBdyTHEARCAA(B *df, B *sf)
{
register L n; register B *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t += tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyADDAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t += tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyADDSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t += tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyADDAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 += tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t -= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdySUBAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t -= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdySUBSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t -= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdySUBAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 -= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t *= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyMULAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t *= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyMULSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t *= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyMULAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 *= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t /= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyDIVAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t /= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyDIVSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t /= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyDIVAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 /= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = pow(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyPWRAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = pow(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyPWRSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = pow(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyPWRAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = pow(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = fmod(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyMODAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fmod(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyMODSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = fmod(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyMODAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WWdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = thearc(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = thearc(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WWdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = thearc(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WWdyTHEARCAA(B *df, B *sf)
{
register L n; register W *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t += tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyADDAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t += tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyADDSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t += tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyADDAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 += tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t -= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdySUBAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t -= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdySUBSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t -= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdySUBAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 -= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t *= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyMULAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t *= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyMULSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t *= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyMULAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 *= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t /= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyDIVAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t /= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyDIVSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t /= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyDIVAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 /= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = pow(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyPWRAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = pow(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyPWRSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = pow(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyPWRAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = pow(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = fmod(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyMODAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fmod(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyMODSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = fmod(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyMODAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WLdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = thearc(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = thearc(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WLdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = thearc(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WLdyTHEARCAA(B *df, B *sf)
{
register L n; register L *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t += tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyADDAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t += tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyADDSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t += tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyADDAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 += tt2;
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 += tt3;
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 += tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t -= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdySUBAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t -= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdySUBSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t -= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdySUBAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 -= tt2;
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 -= tt3;
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 -= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t *= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyMULAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t *= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyMULSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t *= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyMULAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 *= tt2;
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 *= tt3;
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 *= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t /= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyDIVAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t /= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyDIVSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t /= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyDIVAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 /= tt2;
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 /= tt3;
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 /= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = pow(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyPWRAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = pow(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyPWRSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = pow(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyPWRAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = pow(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = fmod(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyMODAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fmod(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyMODSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = fmod(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyMODAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WSdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = thearc(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = thearc(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WSdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = thearc(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WSdyTHEARCAA(B *df, B *sf)
{
register L n; register S *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t += tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyADDAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t += tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyADDSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t += tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyADDAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 += tt2;
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 += tt3;
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 += tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 += tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t -= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdySUBAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t -= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdySUBSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t -= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdySUBAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 -= tt2;
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 -= tt3;
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 -= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 -= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t *= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyMULAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t *= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyMULSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t *= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyMULAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 *= tt2;
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 *= tt3;
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 *= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 *= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t /= tt;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyDIVAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t /= tt;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyDIVSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t /= tt;
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyDIVAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 /= tt2;
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 /= tt3;
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 /= tt4;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 /= tt1;
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = pow(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyPWRAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = pow(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyPWRSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = pow(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyPWRAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = pow(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = pow(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = fmod(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyMODAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fmod(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyMODSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = fmod(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyMODAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void WDdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = thearc(t,tt);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; W *d;
d = (W *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = thearc(t,tt);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WDdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = thearc(t,tt);
}
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WDdyTHEARCAA(B *df, B *sf)
{
register L n; register D *s; register W *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (W *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((W *)(d+1))) == WINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((W *)(d+2))) == WINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((W *)(d+3))) == WINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
*((W *)d++) = (((t2) > WMAX) || ((-t2) < -WMAX) || (t2 == DINF))? WINF : t2;
*((W *)d++) = (((t3) > WMAX) || ((-t3) < -WMAX) || (t3 == DINF))? WINF : t3;
*((W *)d++) = (((t4) > WMAX) || ((-t4) < -WMAX) || (t4 == DINF))? WINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((W *)d)) == WINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((W *)d++) = (((t1) > WMAX) || ((-t1) < -WMAX) || (t1 == DINF))? WINF : t1;
}
}

static void LBdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t += tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyADDAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t += tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyADDSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t += tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyADDAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 += tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t -= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdySUBAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t -= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdySUBSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t -= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdySUBAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 -= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t *= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyMULAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t *= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyMULSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t *= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyMULAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 *= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t /= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyDIVAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t /= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyDIVSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t /= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyDIVAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 /= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = pow(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyPWRAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = pow(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyPWRSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = pow(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyPWRAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = pow(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = fmod(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyMODAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fmod(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyMODSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = fmod(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyMODAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LBdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = thearc(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = thearc(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LBdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = thearc(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LBdyTHEARCAA(B *df, B *sf)
{
register L n; register B *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t += tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyADDAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t += tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyADDSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t += tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyADDAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 += tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t -= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdySUBAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t -= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdySUBSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t -= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdySUBAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 -= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t *= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyMULAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t *= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyMULSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t *= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyMULAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 *= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t /= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyDIVAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t /= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyDIVSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t /= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyDIVAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 /= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = pow(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyPWRAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = pow(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyPWRSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = pow(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyPWRAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = pow(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = fmod(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyMODAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fmod(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyMODSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = fmod(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyMODAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LWdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = thearc(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = thearc(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LWdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = thearc(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LWdyTHEARCAA(B *df, B *sf)
{
register L n; register W *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t += tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyADDAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t += tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyADDSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t += tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyADDAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 += tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 += tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 += tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t -= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdySUBAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t -= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdySUBSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t -= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdySUBAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 -= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 -= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 -= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t *= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyMULAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t *= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyMULSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t *= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyMULAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 *= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 *= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 *= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t /= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyDIVAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t /= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyDIVSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t /= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyDIVAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 /= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 /= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 /= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = pow(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyPWRAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = pow(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyPWRSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = pow(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyPWRAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = pow(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = pow(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = pow(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = fmod(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyMODAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fmod(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyMODSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = fmod(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyMODAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = fmod(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = fmod(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LLdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = thearc(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = thearc(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LLdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = thearc(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LLdyTHEARCAA(B *df, B *sf)
{
register L n; register L *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = thearc(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = thearc(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t += tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyADDAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t += tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyADDSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t += tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyADDAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 += tt2;
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 += tt3;
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 += tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t -= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdySUBAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t -= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdySUBSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t -= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdySUBAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 -= tt2;
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 -= tt3;
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 -= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t *= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyMULAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t *= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyMULSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t *= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyMULAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 *= tt2;
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 *= tt3;
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 *= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t /= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyDIVAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t /= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyDIVSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t /= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyDIVAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 /= tt2;
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 /= tt3;
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 /= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = pow(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyPWRAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = pow(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyPWRSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = pow(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyPWRAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = pow(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = fmod(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyMODAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fmod(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyMODSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = fmod(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyMODAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LSdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((S *)NUM_VAL(sf));
t = thearc(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = thearc(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LSdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = thearc(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LSdyTHEARCAA(B *df, B *sf)
{
register L n; register S *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((S *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((S *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((S *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyADDSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t += tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyADDAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t += tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyADDSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t += tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyADDAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 += tt2;
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 += tt3;
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 += tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 += tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdySUBSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t -= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdySUBAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t -= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdySUBSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t -= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdySUBAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 -= tt2;
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 -= tt3;
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 -= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 -= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyMULSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t *= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyMULAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t *= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyMULSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t *= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyMULAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 *= tt2;
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 *= tt3;
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 *= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 *= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyDIVSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t /= tt;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyDIVAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t /= tt;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyDIVSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t /= tt;
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyDIVAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 /= tt2;
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 /= tt3;
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 /= tt4;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 /= tt1;
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyPWRSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = pow(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyPWRAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = pow(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyPWRSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = pow(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyPWRAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = pow(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = pow(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = pow(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = pow(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyMODSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = fmod(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyMODAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fmod(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyMODSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = fmod(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyMODAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = fmod(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = fmod(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = fmod(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = fmod(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void LDdyTHEARCSS(B *df, B *sf)
{
D t, tt;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
tt = *((D *)NUM_VAL(sf));
t = thearc(t,tt);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; L *d;
d = (L *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = thearc(t,tt);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LDdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = thearc(t,tt);
}
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LDdyTHEARCAA(B *df, B *sf)
{
register L n; register D *s; register L *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (L *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
tt2 = *((D *)s++);
if ((t2 = *((L *)(d+1))) == LINF) t2 = DINF;
t2 = thearc(t2,tt2);
tt3 = *((D *)s++);
if ((t3 = *((L *)(d+2))) == LINF) t3 = DINF;
t3 = thearc(t3,tt3);
tt4 = *((D *)s++);
if ((t4 = *((L *)(d+3))) == LINF) t4 = DINF;
t4 = thearc(t4,tt4);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
*((L *)d++) = (((t2) > LMAX) || ((-t2) < -LMAX) || (t2 == DINF))? LINF : t2;
*((L *)d++) = (((t3) > LMAX) || ((-t3) < -LMAX) || (t3 == DINF))? LINF : t3;
*((L *)d++) = (((t4) > LMAX) || ((-t4) < -LMAX) || (t4 == DINF))? LINF : t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
if ((t1 = *((L *)d)) == LINF) t1 = DINF;
t1 = thearc(t1,tt1);
*((L *)d++) = (((t1) > LMAX) || ((-t1) < -LMAX) || (t1 == DINF))? LINF : t1;
}
}

static void SBdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t += tt;
*((S *)NUM_VAL(df)) = t;
}

static void SBdyADDAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t += tt;
*((S *)d++) = t;
}
}

static void SBdyADDSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t += tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyADDAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 += tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 += tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 += tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
*((S *)d++) = t1;
}
}

static void SBdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t -= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SBdySUBAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t -= tt;
*((S *)d++) = t;
}
}

static void SBdySUBSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t -= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdySUBAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 -= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 -= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 -= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
*((S *)d++) = t1;
}
}

static void SBdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t *= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SBdyMULAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t *= tt;
*((S *)d++) = t;
}
}

static void SBdyMULSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t *= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyMULAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 *= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 *= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 *= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
*((S *)d++) = t1;
}
}

static void SBdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t /= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SBdyDIVAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t /= tt;
*((S *)d++) = t;
}
}

static void SBdyDIVSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t /= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyDIVAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 /= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 /= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 /= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
*((S *)d++) = t1;
}
}

static void SBdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = pow(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SBdyPWRAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = pow(t,tt);
*((S *)d++) = t;
}
}

static void SBdyPWRSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = pow(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyPWRAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = pow(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
*((S *)d++) = t1;
}
}

static void SBdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = fmod(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SBdyMODAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fmod(t,tt);
*((S *)d++) = t;
}
}

static void SBdyMODSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = fmod(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyMODAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = fmod(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
*((S *)d++) = t1;
}
}

static void SBdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = thearc(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SBdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = thearc(t,tt);
*((S *)d++) = t;
}
}

static void SBdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = thearc(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SBdyTHEARCAA(B *df, B *sf)
{
register L n; register B *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = thearc(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
*((S *)d++) = t1;
}
}

static void SWdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t += tt;
*((S *)NUM_VAL(df)) = t;
}

static void SWdyADDAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t += tt;
*((S *)d++) = t;
}
}

static void SWdyADDSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t += tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyADDAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 += tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 += tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 += tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
*((S *)d++) = t1;
}
}

static void SWdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t -= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SWdySUBAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t -= tt;
*((S *)d++) = t;
}
}

static void SWdySUBSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t -= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdySUBAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 -= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 -= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 -= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
*((S *)d++) = t1;
}
}

static void SWdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t *= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SWdyMULAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t *= tt;
*((S *)d++) = t;
}
}

static void SWdyMULSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t *= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyMULAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 *= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 *= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 *= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
*((S *)d++) = t1;
}
}

static void SWdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t /= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SWdyDIVAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t /= tt;
*((S *)d++) = t;
}
}

static void SWdyDIVSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t /= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyDIVAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 /= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 /= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 /= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
*((S *)d++) = t1;
}
}

static void SWdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = pow(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SWdyPWRAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = pow(t,tt);
*((S *)d++) = t;
}
}

static void SWdyPWRSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = pow(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyPWRAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = pow(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
*((S *)d++) = t1;
}
}

static void SWdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = fmod(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SWdyMODAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fmod(t,tt);
*((S *)d++) = t;
}
}

static void SWdyMODSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = fmod(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyMODAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = fmod(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
*((S *)d++) = t1;
}
}

static void SWdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = thearc(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SWdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = thearc(t,tt);
*((S *)d++) = t;
}
}

static void SWdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = thearc(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SWdyTHEARCAA(B *df, B *sf)
{
register L n; register W *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = thearc(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
*((S *)d++) = t1;
}
}

static void SLdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t += tt;
*((S *)NUM_VAL(df)) = t;
}

static void SLdyADDAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t += tt;
*((S *)d++) = t;
}
}

static void SLdyADDSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t += tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyADDAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 += tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 += tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 += tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 += tt1;
*((S *)d++) = t1;
}
}

static void SLdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t -= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SLdySUBAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t -= tt;
*((S *)d++) = t;
}
}

static void SLdySUBSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t -= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdySUBAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 -= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 -= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 -= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 -= tt1;
*((S *)d++) = t1;
}
}

static void SLdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t *= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SLdyMULAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t *= tt;
*((S *)d++) = t;
}
}

static void SLdyMULSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t *= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyMULAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 *= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 *= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 *= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 *= tt1;
*((S *)d++) = t1;
}
}

static void SLdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t /= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SLdyDIVAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t /= tt;
*((S *)d++) = t;
}
}

static void SLdyDIVSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t /= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyDIVAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 /= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 /= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 /= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 /= tt1;
*((S *)d++) = t1;
}
}

static void SLdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = pow(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SLdyPWRAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = pow(t,tt);
*((S *)d++) = t;
}
}

static void SLdyPWRSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = pow(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyPWRAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = pow(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = pow(t1,tt1);
*((S *)d++) = t1;
}
}

static void SLdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = fmod(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SLdyMODAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fmod(t,tt);
*((S *)d++) = t;
}
}

static void SLdyMODSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = fmod(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyMODAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = fmod(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = fmod(t1,tt1);
*((S *)d++) = t1;
}
}

static void SLdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = thearc(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SLdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = thearc(t,tt);
*((S *)d++) = t;
}
}

static void SLdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = thearc(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SLdyTHEARCAA(B *df, B *sf)
{
register L n; register L *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((S *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((S *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((S *)(d+3));
t4 = thearc(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((S *)d);
t1 = thearc(t1,tt1);
*((S *)d++) = t1;
}
}

static void SSdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t += tt;
*((S *)NUM_VAL(df)) = t;
}

static void SSdyADDAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t += tt;
*((S *)d++) = t;
}
}

static void SSdyADDSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t += tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyADDAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 += tt1;
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 += tt2;
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 += tt3;
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 += tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 += tt1;
*((S *)d++) = t1;
}
}

static void SSdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t -= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SSdySUBAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t -= tt;
*((S *)d++) = t;
}
}

static void SSdySUBSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t -= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdySUBAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 -= tt1;
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 -= tt2;
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 -= tt3;
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 -= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 -= tt1;
*((S *)d++) = t1;
}
}

static void SSdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t *= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SSdyMULAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t *= tt;
*((S *)d++) = t;
}
}

static void SSdyMULSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t *= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyMULAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 *= tt1;
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 *= tt2;
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 *= tt3;
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 *= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 *= tt1;
*((S *)d++) = t1;
}
}

static void SSdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t /= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SSdyDIVAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t /= tt;
*((S *)d++) = t;
}
}

static void SSdyDIVSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t /= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyDIVAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 /= tt1;
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 /= tt2;
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 /= tt3;
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 /= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 /= tt1;
*((S *)d++) = t1;
}
}

static void SSdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = pow(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SSdyPWRAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = pow(t,tt);
*((S *)d++) = t;
}
}

static void SSdyPWRSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = pow(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyPWRAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = pow(t1,tt1);
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 = pow(t2,tt2);
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 = pow(t3,tt3);
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 = pow(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = pow(t1,tt1);
*((S *)d++) = t1;
}
}

static void SSdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = fmod(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SSdyMODAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fmod(t,tt);
*((S *)d++) = t;
}
}

static void SSdyMODSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = fmod(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyMODAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = fmod(t1,tt1);
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 = fmod(t2,tt2);
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 = fmod(t3,tt3);
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 = fmod(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = fmod(t1,tt1);
*((S *)d++) = t1;
}
}

static void SSdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = thearc(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SSdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = thearc(t,tt);
*((S *)d++) = t;
}
}

static void SSdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = thearc(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SSdyTHEARCAA(B *df, B *sf)
{
register L n; register S *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = thearc(t1,tt1);
tt2 = *((S *)s++);
t2 = *((S *)(d+1));
t2 = thearc(t2,tt2);
tt3 = *((S *)s++);
t3 = *((S *)(d+2));
t3 = thearc(t3,tt3);
tt4 = *((S *)s++);
t4 = *((S *)(d+3));
t4 = thearc(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((S *)d);
t1 = thearc(t1,tt1);
*((S *)d++) = t1;
}
}

static void SDdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t += tt;
*((S *)NUM_VAL(df)) = t;
}

static void SDdyADDAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t += tt;
*((S *)d++) = t;
}
}

static void SDdyADDSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t += tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyADDAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 += tt1;
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 += tt2;
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 += tt3;
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 += tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 += tt1;
*((S *)d++) = t1;
}
}

static void SDdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t -= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SDdySUBAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t -= tt;
*((S *)d++) = t;
}
}

static void SDdySUBSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t -= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdySUBAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 -= tt1;
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 -= tt2;
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 -= tt3;
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 -= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 -= tt1;
*((S *)d++) = t1;
}
}

static void SDdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t *= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SDdyMULAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t *= tt;
*((S *)d++) = t;
}
}

static void SDdyMULSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t *= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyMULAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 *= tt1;
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 *= tt2;
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 *= tt3;
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 *= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 *= tt1;
*((S *)d++) = t1;
}
}

static void SDdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t /= tt;
*((S *)NUM_VAL(df)) = t;
}

static void SDdyDIVAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t /= tt;
*((S *)d++) = t;
}
}

static void SDdyDIVSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t /= tt;
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyDIVAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 /= tt1;
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 /= tt2;
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 /= tt3;
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 /= tt4;
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 /= tt1;
*((S *)d++) = t1;
}
}

static void SDdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = pow(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SDdyPWRAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = pow(t,tt);
*((S *)d++) = t;
}
}

static void SDdyPWRSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = pow(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyPWRAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = pow(t1,tt1);
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 = pow(t2,tt2);
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 = pow(t3,tt3);
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 = pow(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = pow(t1,tt1);
*((S *)d++) = t1;
}
}

static void SDdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = fmod(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SDdyMODAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fmod(t,tt);
*((S *)d++) = t;
}
}

static void SDdyMODSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = fmod(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyMODAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = fmod(t1,tt1);
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 = fmod(t2,tt2);
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 = fmod(t3,tt3);
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 = fmod(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = fmod(t1,tt1);
*((S *)d++) = t1;
}
}

static void SDdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((S *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = thearc(t,tt);
*((S *)NUM_VAL(df)) = t;
}

static void SDdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; S *d;
d = (S *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = thearc(t,tt);
*((S *)d++) = t;
}
}

static void SDdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((S *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = thearc(t,tt);
}
*((S *)NUM_VAL(df)) = t;
}

static void SDdyTHEARCAA(B *df, B *sf)
{
register L n; register D *s; register S *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (S *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = thearc(t1,tt1);
tt2 = *((D *)s++);
t2 = *((S *)(d+1));
t2 = thearc(t2,tt2);
tt3 = *((D *)s++);
t3 = *((S *)(d+2));
t3 = thearc(t3,tt3);
tt4 = *((D *)s++);
t4 = *((S *)(d+3));
t4 = thearc(t4,tt4);
*((S *)d++) = t1;
*((S *)d++) = t2;
*((S *)d++) = t3;
*((S *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((S *)d);
t1 = thearc(t1,tt1);
*((S *)d++) = t1;
}
}

static void DBdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t += tt;
*((D *)NUM_VAL(df)) = t;
}

static void DBdyADDAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t += tt;
*((D *)d++) = t;
}
}

static void DBdyADDSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t += tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyADDAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 += tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 += tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 += tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
*((D *)d++) = t1;
}
}

static void DBdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t -= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DBdySUBAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t -= tt;
*((D *)d++) = t;
}
}

static void DBdySUBSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t -= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdySUBAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 -= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 -= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 -= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
*((D *)d++) = t1;
}
}

static void DBdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t *= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DBdyMULAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t *= tt;
*((D *)d++) = t;
}
}

static void DBdyMULSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t *= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyMULAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 *= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 *= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 *= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
*((D *)d++) = t1;
}
}

static void DBdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t /= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DBdyDIVAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t /= tt;
*((D *)d++) = t;
}
}

static void DBdyDIVSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t /= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyDIVAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 /= tt2;
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 /= tt3;
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 /= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
*((D *)d++) = t1;
}
}

static void DBdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = pow(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DBdyPWRAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = pow(t,tt);
*((D *)d++) = t;
}
}

static void DBdyPWRSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = pow(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyPWRAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = pow(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
*((D *)d++) = t1;
}
}

static void DBdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = fmod(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DBdyMODAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fmod(t,tt);
*((D *)d++) = t;
}
}

static void DBdyMODSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = fmod(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyMODAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = fmod(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
*((D *)d++) = t1;
}
}

static void DBdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
t = thearc(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DBdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((B *)NUM_VAL(sf))) == BINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = thearc(t,tt);
*((D *)d++) = t;
}
}

static void DBdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; B *s;
s = (B *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((B *)s++)) == BINF) tt = DINF;
t = thearc(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DBdyTHEARCAA(B *df, B *sf)
{
register L n; register B *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (B *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((B *)s++)) == BINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((B *)s++)) == BINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((B *)s++)) == BINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = thearc(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((B *)s++)) == BINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
*((D *)d++) = t1;
}
}

static void DWdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t += tt;
*((D *)NUM_VAL(df)) = t;
}

static void DWdyADDAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t += tt;
*((D *)d++) = t;
}
}

static void DWdyADDSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t += tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyADDAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 += tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 += tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 += tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
*((D *)d++) = t1;
}
}

static void DWdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t -= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DWdySUBAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t -= tt;
*((D *)d++) = t;
}
}

static void DWdySUBSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t -= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdySUBAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 -= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 -= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 -= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
*((D *)d++) = t1;
}
}

static void DWdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t *= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DWdyMULAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t *= tt;
*((D *)d++) = t;
}
}

static void DWdyMULSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t *= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyMULAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 *= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 *= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 *= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
*((D *)d++) = t1;
}
}

static void DWdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t /= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DWdyDIVAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t /= tt;
*((D *)d++) = t;
}
}

static void DWdyDIVSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t /= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyDIVAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 /= tt2;
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 /= tt3;
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 /= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
*((D *)d++) = t1;
}
}

static void DWdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = pow(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DWdyPWRAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = pow(t,tt);
*((D *)d++) = t;
}
}

static void DWdyPWRSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = pow(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyPWRAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = pow(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
*((D *)d++) = t1;
}
}

static void DWdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = fmod(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DWdyMODAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fmod(t,tt);
*((D *)d++) = t;
}
}

static void DWdyMODSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = fmod(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyMODAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = fmod(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
*((D *)d++) = t1;
}
}

static void DWdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
t = thearc(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DWdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((W *)NUM_VAL(sf))) == WINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = thearc(t,tt);
*((D *)d++) = t;
}
}

static void DWdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; W *s;
s = (W *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((W *)s++)) == WINF) tt = DINF;
t = thearc(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DWdyTHEARCAA(B *df, B *sf)
{
register L n; register W *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (W *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((W *)s++)) == WINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((W *)s++)) == WINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((W *)s++)) == WINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = thearc(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((W *)s++)) == WINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
*((D *)d++) = t1;
}
}

static void DLdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t += tt;
*((D *)NUM_VAL(df)) = t;
}

static void DLdyADDAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t += tt;
*((D *)d++) = t;
}
}

static void DLdyADDSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t += tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyADDAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 += tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 += tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 += tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 += tt1;
*((D *)d++) = t1;
}
}

static void DLdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t -= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DLdySUBAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t -= tt;
*((D *)d++) = t;
}
}

static void DLdySUBSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t -= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdySUBAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 -= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 -= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 -= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 -= tt1;
*((D *)d++) = t1;
}
}

static void DLdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t *= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DLdyMULAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t *= tt;
*((D *)d++) = t;
}
}

static void DLdyMULSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t *= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyMULAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 *= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 *= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 *= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 *= tt1;
*((D *)d++) = t1;
}
}

static void DLdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t /= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DLdyDIVAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t /= tt;
*((D *)d++) = t;
}
}

static void DLdyDIVSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t /= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyDIVAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 /= tt2;
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 /= tt3;
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 /= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 /= tt1;
*((D *)d++) = t1;
}
}

static void DLdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = pow(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DLdyPWRAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = pow(t,tt);
*((D *)d++) = t;
}
}

static void DLdyPWRSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = pow(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyPWRAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = pow(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = pow(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = pow(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = pow(t1,tt1);
*((D *)d++) = t1;
}
}

static void DLdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = fmod(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DLdyMODAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fmod(t,tt);
*((D *)d++) = t;
}
}

static void DLdyMODSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = fmod(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyMODAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = fmod(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = fmod(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = fmod(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = fmod(t1,tt1);
*((D *)d++) = t1;
}
}

static void DLdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
t = thearc(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DLdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
if ((tt = *((L *)NUM_VAL(sf))) == LINF) tt = DINF;
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = thearc(t,tt);
*((D *)d++) = t;
}
}

static void DLdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; L *s;
s = (L *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
if ((tt = *((L *)s++)) == LINF) tt = DINF;
t = thearc(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DLdyTHEARCAA(B *df, B *sf)
{
register L n; register L *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (L *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
if ((tt2 = *((L *)s++)) == LINF) tt2 = DINF;
t2 = *((D *)(d+1));
t2 = thearc(t2,tt2);
if ((tt3 = *((L *)s++)) == LINF) tt3 = DINF;
t3 = *((D *)(d+2));
t3 = thearc(t3,tt3);
if ((tt4 = *((L *)s++)) == LINF) tt4 = DINF;
t4 = *((D *)(d+3));
t4 = thearc(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
if ((tt1 = *((L *)s++)) == LINF) tt1 = DINF;
t1 = *((D *)d);
t1 = thearc(t1,tt1);
*((D *)d++) = t1;
}
}

static void DSdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t += tt;
*((D *)NUM_VAL(df)) = t;
}

static void DSdyADDAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t += tt;
*((D *)d++) = t;
}
}

static void DSdyADDSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t += tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyADDAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 += tt1;
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 += tt2;
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 += tt3;
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 += tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 += tt1;
*((D *)d++) = t1;
}
}

static void DSdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t -= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DSdySUBAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t -= tt;
*((D *)d++) = t;
}
}

static void DSdySUBSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t -= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdySUBAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 -= tt1;
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 -= tt2;
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 -= tt3;
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 -= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 -= tt1;
*((D *)d++) = t1;
}
}

static void DSdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t *= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DSdyMULAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t *= tt;
*((D *)d++) = t;
}
}

static void DSdyMULSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t *= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyMULAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 *= tt1;
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 *= tt2;
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 *= tt3;
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 *= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 *= tt1;
*((D *)d++) = t1;
}
}

static void DSdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t /= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DSdyDIVAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t /= tt;
*((D *)d++) = t;
}
}

static void DSdyDIVSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t /= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyDIVAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 /= tt1;
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 /= tt2;
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 /= tt3;
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 /= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 /= tt1;
*((D *)d++) = t1;
}
}

static void DSdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = pow(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DSdyPWRAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = pow(t,tt);
*((D *)d++) = t;
}
}

static void DSdyPWRSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = pow(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyPWRAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = pow(t1,tt1);
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 = pow(t2,tt2);
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 = pow(t3,tt3);
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 = pow(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = pow(t1,tt1);
*((D *)d++) = t1;
}
}

static void DSdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = fmod(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DSdyMODAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fmod(t,tt);
*((D *)d++) = t;
}
}

static void DSdyMODSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = fmod(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyMODAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = fmod(t1,tt1);
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 = fmod(t2,tt2);
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 = fmod(t3,tt3);
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 = fmod(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = fmod(t1,tt1);
*((D *)d++) = t1;
}
}

static void DSdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((S *)NUM_VAL(sf));
t = thearc(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DSdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((S *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = thearc(t,tt);
*((D *)d++) = t;
}
}

static void DSdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; S *s;
s = (S *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((S *)s++);
t = thearc(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DSdyTHEARCAA(B *df, B *sf)
{
register L n; register S *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (S *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = thearc(t1,tt1);
tt2 = *((S *)s++);
t2 = *((D *)(d+1));
t2 = thearc(t2,tt2);
tt3 = *((S *)s++);
t3 = *((D *)(d+2));
t3 = thearc(t3,tt3);
tt4 = *((S *)s++);
t4 = *((D *)(d+3));
t4 = thearc(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((S *)s++);
t1 = *((D *)d);
t1 = thearc(t1,tt1);
*((D *)d++) = t1;
}
}

static void DDdyADDSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t += tt;
*((D *)NUM_VAL(df)) = t;
}

static void DDdyADDAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t += tt;
*((D *)d++) = t;
}
}

static void DDdyADDSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t += tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyADDAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 += tt1;
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 += tt2;
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 += tt3;
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 += tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 += tt1;
*((D *)d++) = t1;
}
}

static void DDdySUBSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t -= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DDdySUBAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t -= tt;
*((D *)d++) = t;
}
}

static void DDdySUBSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t -= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdySUBAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 -= tt1;
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 -= tt2;
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 -= tt3;
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 -= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 -= tt1;
*((D *)d++) = t1;
}
}

static void DDdyMULSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t *= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DDdyMULAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t *= tt;
*((D *)d++) = t;
}
}

static void DDdyMULSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t *= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyMULAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 *= tt1;
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 *= tt2;
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 *= tt3;
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 *= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 *= tt1;
*((D *)d++) = t1;
}
}

static void DDdyDIVSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t /= tt;
*((D *)NUM_VAL(df)) = t;
}

static void DDdyDIVAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t /= tt;
*((D *)d++) = t;
}
}

static void DDdyDIVSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t /= tt;
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyDIVAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 /= tt1;
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 /= tt2;
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 /= tt3;
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 /= tt4;
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 /= tt1;
*((D *)d++) = t1;
}
}

static void DDdyPWRSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = pow(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DDdyPWRAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = pow(t,tt);
*((D *)d++) = t;
}
}

static void DDdyPWRSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = pow(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyPWRAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = pow(t1,tt1);
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 = pow(t2,tt2);
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 = pow(t3,tt3);
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 = pow(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = pow(t1,tt1);
*((D *)d++) = t1;
}
}

static void DDdyMODSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = fmod(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DDdyMODAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fmod(t,tt);
*((D *)d++) = t;
}
}

static void DDdyMODSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = fmod(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyMODAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = fmod(t1,tt1);
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 = fmod(t2,tt2);
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 = fmod(t3,tt3);
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 = fmod(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = fmod(t1,tt1);
*((D *)d++) = t1;
}
}

static void DDdyTHEARCSS(B *df, B *sf)
{
D t, tt;
t = *((D *)NUM_VAL(df));
tt = *((D *)NUM_VAL(sf));
t = thearc(t,tt);
*((D *)NUM_VAL(df)) = t;
}

static void DDdyTHEARCAS(B *df, B *sf)
{
D t,tt; L n; D *d;
d = (D *)VALUE_BASE(df);
tt = *((D *)NUM_VAL(sf));
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = thearc(t,tt);
*((D *)d++) = t;
}
}

static void DDdyTHEARCSA(B *df, B *sf)
{
D t,tt; L n; D *s;
s = (D *)VALUE_BASE(sf);
t = *((D *)NUM_VAL(df));
for (n = ARRAY_SIZE(sf); n>0; n--) {
tt = *((D *)s++);
t = thearc(t,tt);
}
*((D *)NUM_VAL(df)) = t;
}

static void DDdyTHEARCAA(B *df, B *sf)
{
register L n; register D *s; register D *d;
register D t1,t2,t3,t4,tt1,tt2,tt3,tt4; 
s = (D *)VALUE_BASE(sf);
d = (D *)VALUE_BASE(df);
for (n = (ARRAY_SIZE(df)>>2); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = thearc(t1,tt1);
tt2 = *((D *)s++);
t2 = *((D *)(d+1));
t2 = thearc(t2,tt2);
tt3 = *((D *)s++);
t3 = *((D *)(d+2));
t3 = thearc(t3,tt3);
tt4 = *((D *)s++);
t4 = *((D *)(d+3));
t4 = thearc(t4,tt4);
*((D *)d++) = t1;
*((D *)d++) = t2;
*((D *)d++) = t3;
*((D *)d++) = t4;
}
for (n = (ARRAY_SIZE(df)&3); n>0; n--) {
tt1 = *((D *)s++);
t1 = *((D *)d);
t1 = thearc(t1,tt1);
*((D *)d++) = t1;
}
}

typedef void (*dyadic_fct)(B*,B*);
static dyadic_fct ADDlist[] = {
BBdyADDSS, BBdyADDAS, BBdyADDSA, BBdyADDAA, 
BWdyADDSS, BWdyADDAS, BWdyADDSA, BWdyADDAA, 
BLdyADDSS, BLdyADDAS, BLdyADDSA, BLdyADDAA, 
BSdyADDSS, BSdyADDAS, BSdyADDSA, BSdyADDAA, 
BDdyADDSS, BDdyADDAS, BDdyADDSA, BDdyADDAA, 
WBdyADDSS, WBdyADDAS, WBdyADDSA, WBdyADDAA, 
WWdyADDSS, WWdyADDAS, WWdyADDSA, WWdyADDAA, 
WLdyADDSS, WLdyADDAS, WLdyADDSA, WLdyADDAA, 
WSdyADDSS, WSdyADDAS, WSdyADDSA, WSdyADDAA, 
WDdyADDSS, WDdyADDAS, WDdyADDSA, WDdyADDAA, 
LBdyADDSS, LBdyADDAS, LBdyADDSA, LBdyADDAA, 
LWdyADDSS, LWdyADDAS, LWdyADDSA, LWdyADDAA, 
LLdyADDSS, LLdyADDAS, LLdyADDSA, LLdyADDAA, 
LSdyADDSS, LSdyADDAS, LSdyADDSA, LSdyADDAA, 
LDdyADDSS, LDdyADDAS, LDdyADDSA, LDdyADDAA, 
SBdyADDSS, SBdyADDAS, SBdyADDSA, SBdyADDAA, 
SWdyADDSS, SWdyADDAS, SWdyADDSA, SWdyADDAA, 
SLdyADDSS, SLdyADDAS, SLdyADDSA, SLdyADDAA, 
SSdyADDSS, SSdyADDAS, SSdyADDSA, SSdyADDAA, 
SDdyADDSS, SDdyADDAS, SDdyADDSA, SDdyADDAA, 
DBdyADDSS, DBdyADDAS, DBdyADDSA, DBdyADDAA, 
DWdyADDSS, DWdyADDAS, DWdyADDSA, DWdyADDAA, 
DLdyADDSS, DLdyADDAS, DLdyADDSA, DLdyADDAA, 
DSdyADDSS, DSdyADDAS, DSdyADDSA, DSdyADDAA, 
DDdyADDSS, DDdyADDAS, DDdyADDSA, DDdyADDAA, 

};
static dyadic_fct SUBlist[] = {
BBdySUBSS, BBdySUBAS, BBdySUBSA, BBdySUBAA, 
BWdySUBSS, BWdySUBAS, BWdySUBSA, BWdySUBAA, 
BLdySUBSS, BLdySUBAS, BLdySUBSA, BLdySUBAA, 
BSdySUBSS, BSdySUBAS, BSdySUBSA, BSdySUBAA, 
BDdySUBSS, BDdySUBAS, BDdySUBSA, BDdySUBAA, 
WBdySUBSS, WBdySUBAS, WBdySUBSA, WBdySUBAA, 
WWdySUBSS, WWdySUBAS, WWdySUBSA, WWdySUBAA, 
WLdySUBSS, WLdySUBAS, WLdySUBSA, WLdySUBAA, 
WSdySUBSS, WSdySUBAS, WSdySUBSA, WSdySUBAA, 
WDdySUBSS, WDdySUBAS, WDdySUBSA, WDdySUBAA, 
LBdySUBSS, LBdySUBAS, LBdySUBSA, LBdySUBAA, 
LWdySUBSS, LWdySUBAS, LWdySUBSA, LWdySUBAA, 
LLdySUBSS, LLdySUBAS, LLdySUBSA, LLdySUBAA, 
LSdySUBSS, LSdySUBAS, LSdySUBSA, LSdySUBAA, 
LDdySUBSS, LDdySUBAS, LDdySUBSA, LDdySUBAA, 
SBdySUBSS, SBdySUBAS, SBdySUBSA, SBdySUBAA, 
SWdySUBSS, SWdySUBAS, SWdySUBSA, SWdySUBAA, 
SLdySUBSS, SLdySUBAS, SLdySUBSA, SLdySUBAA, 
SSdySUBSS, SSdySUBAS, SSdySUBSA, SSdySUBAA, 
SDdySUBSS, SDdySUBAS, SDdySUBSA, SDdySUBAA, 
DBdySUBSS, DBdySUBAS, DBdySUBSA, DBdySUBAA, 
DWdySUBSS, DWdySUBAS, DWdySUBSA, DWdySUBAA, 
DLdySUBSS, DLdySUBAS, DLdySUBSA, DLdySUBAA, 
DSdySUBSS, DSdySUBAS, DSdySUBSA, DSdySUBAA, 
DDdySUBSS, DDdySUBAS, DDdySUBSA, DDdySUBAA, 

};
static dyadic_fct MULlist[] = {
BBdyMULSS, BBdyMULAS, BBdyMULSA, BBdyMULAA, 
BWdyMULSS, BWdyMULAS, BWdyMULSA, BWdyMULAA, 
BLdyMULSS, BLdyMULAS, BLdyMULSA, BLdyMULAA, 
BSdyMULSS, BSdyMULAS, BSdyMULSA, BSdyMULAA, 
BDdyMULSS, BDdyMULAS, BDdyMULSA, BDdyMULAA, 
WBdyMULSS, WBdyMULAS, WBdyMULSA, WBdyMULAA, 
WWdyMULSS, WWdyMULAS, WWdyMULSA, WWdyMULAA, 
WLdyMULSS, WLdyMULAS, WLdyMULSA, WLdyMULAA, 
WSdyMULSS, WSdyMULAS, WSdyMULSA, WSdyMULAA, 
WDdyMULSS, WDdyMULAS, WDdyMULSA, WDdyMULAA, 
LBdyMULSS, LBdyMULAS, LBdyMULSA, LBdyMULAA, 
LWdyMULSS, LWdyMULAS, LWdyMULSA, LWdyMULAA, 
LLdyMULSS, LLdyMULAS, LLdyMULSA, LLdyMULAA, 
LSdyMULSS, LSdyMULAS, LSdyMULSA, LSdyMULAA, 
LDdyMULSS, LDdyMULAS, LDdyMULSA, LDdyMULAA, 
SBdyMULSS, SBdyMULAS, SBdyMULSA, SBdyMULAA, 
SWdyMULSS, SWdyMULAS, SWdyMULSA, SWdyMULAA, 
SLdyMULSS, SLdyMULAS, SLdyMULSA, SLdyMULAA, 
SSdyMULSS, SSdyMULAS, SSdyMULSA, SSdyMULAA, 
SDdyMULSS, SDdyMULAS, SDdyMULSA, SDdyMULAA, 
DBdyMULSS, DBdyMULAS, DBdyMULSA, DBdyMULAA, 
DWdyMULSS, DWdyMULAS, DWdyMULSA, DWdyMULAA, 
DLdyMULSS, DLdyMULAS, DLdyMULSA, DLdyMULAA, 
DSdyMULSS, DSdyMULAS, DSdyMULSA, DSdyMULAA, 
DDdyMULSS, DDdyMULAS, DDdyMULSA, DDdyMULAA, 

};
static dyadic_fct DIVlist[] = {
BBdyDIVSS, BBdyDIVAS, BBdyDIVSA, BBdyDIVAA, 
BWdyDIVSS, BWdyDIVAS, BWdyDIVSA, BWdyDIVAA, 
BLdyDIVSS, BLdyDIVAS, BLdyDIVSA, BLdyDIVAA, 
BSdyDIVSS, BSdyDIVAS, BSdyDIVSA, BSdyDIVAA, 
BDdyDIVSS, BDdyDIVAS, BDdyDIVSA, BDdyDIVAA, 
WBdyDIVSS, WBdyDIVAS, WBdyDIVSA, WBdyDIVAA, 
WWdyDIVSS, WWdyDIVAS, WWdyDIVSA, WWdyDIVAA, 
WLdyDIVSS, WLdyDIVAS, WLdyDIVSA, WLdyDIVAA, 
WSdyDIVSS, WSdyDIVAS, WSdyDIVSA, WSdyDIVAA, 
WDdyDIVSS, WDdyDIVAS, WDdyDIVSA, WDdyDIVAA, 
LBdyDIVSS, LBdyDIVAS, LBdyDIVSA, LBdyDIVAA, 
LWdyDIVSS, LWdyDIVAS, LWdyDIVSA, LWdyDIVAA, 
LLdyDIVSS, LLdyDIVAS, LLdyDIVSA, LLdyDIVAA, 
LSdyDIVSS, LSdyDIVAS, LSdyDIVSA, LSdyDIVAA, 
LDdyDIVSS, LDdyDIVAS, LDdyDIVSA, LDdyDIVAA, 
SBdyDIVSS, SBdyDIVAS, SBdyDIVSA, SBdyDIVAA, 
SWdyDIVSS, SWdyDIVAS, SWdyDIVSA, SWdyDIVAA, 
SLdyDIVSS, SLdyDIVAS, SLdyDIVSA, SLdyDIVAA, 
SSdyDIVSS, SSdyDIVAS, SSdyDIVSA, SSdyDIVAA, 
SDdyDIVSS, SDdyDIVAS, SDdyDIVSA, SDdyDIVAA, 
DBdyDIVSS, DBdyDIVAS, DBdyDIVSA, DBdyDIVAA, 
DWdyDIVSS, DWdyDIVAS, DWdyDIVSA, DWdyDIVAA, 
DLdyDIVSS, DLdyDIVAS, DLdyDIVSA, DLdyDIVAA, 
DSdyDIVSS, DSdyDIVAS, DSdyDIVSA, DSdyDIVAA, 
DDdyDIVSS, DDdyDIVAS, DDdyDIVSA, DDdyDIVAA, 

};
static dyadic_fct PWRlist[] = {
BBdyPWRSS, BBdyPWRAS, BBdyPWRSA, BBdyPWRAA, 
BWdyPWRSS, BWdyPWRAS, BWdyPWRSA, BWdyPWRAA, 
BLdyPWRSS, BLdyPWRAS, BLdyPWRSA, BLdyPWRAA, 
BSdyPWRSS, BSdyPWRAS, BSdyPWRSA, BSdyPWRAA, 
BDdyPWRSS, BDdyPWRAS, BDdyPWRSA, BDdyPWRAA, 
WBdyPWRSS, WBdyPWRAS, WBdyPWRSA, WBdyPWRAA, 
WWdyPWRSS, WWdyPWRAS, WWdyPWRSA, WWdyPWRAA, 
WLdyPWRSS, WLdyPWRAS, WLdyPWRSA, WLdyPWRAA, 
WSdyPWRSS, WSdyPWRAS, WSdyPWRSA, WSdyPWRAA, 
WDdyPWRSS, WDdyPWRAS, WDdyPWRSA, WDdyPWRAA, 
LBdyPWRSS, LBdyPWRAS, LBdyPWRSA, LBdyPWRAA, 
LWdyPWRSS, LWdyPWRAS, LWdyPWRSA, LWdyPWRAA, 
LLdyPWRSS, LLdyPWRAS, LLdyPWRSA, LLdyPWRAA, 
LSdyPWRSS, LSdyPWRAS, LSdyPWRSA, LSdyPWRAA, 
LDdyPWRSS, LDdyPWRAS, LDdyPWRSA, LDdyPWRAA, 
SBdyPWRSS, SBdyPWRAS, SBdyPWRSA, SBdyPWRAA, 
SWdyPWRSS, SWdyPWRAS, SWdyPWRSA, SWdyPWRAA, 
SLdyPWRSS, SLdyPWRAS, SLdyPWRSA, SLdyPWRAA, 
SSdyPWRSS, SSdyPWRAS, SSdyPWRSA, SSdyPWRAA, 
SDdyPWRSS, SDdyPWRAS, SDdyPWRSA, SDdyPWRAA, 
DBdyPWRSS, DBdyPWRAS, DBdyPWRSA, DBdyPWRAA, 
DWdyPWRSS, DWdyPWRAS, DWdyPWRSA, DWdyPWRAA, 
DLdyPWRSS, DLdyPWRAS, DLdyPWRSA, DLdyPWRAA, 
DSdyPWRSS, DSdyPWRAS, DSdyPWRSA, DSdyPWRAA, 
DDdyPWRSS, DDdyPWRAS, DDdyPWRSA, DDdyPWRAA, 

};
static dyadic_fct MODlist[] = {
BBdyMODSS, BBdyMODAS, BBdyMODSA, BBdyMODAA, 
BWdyMODSS, BWdyMODAS, BWdyMODSA, BWdyMODAA, 
BLdyMODSS, BLdyMODAS, BLdyMODSA, BLdyMODAA, 
BSdyMODSS, BSdyMODAS, BSdyMODSA, BSdyMODAA, 
BDdyMODSS, BDdyMODAS, BDdyMODSA, BDdyMODAA, 
WBdyMODSS, WBdyMODAS, WBdyMODSA, WBdyMODAA, 
WWdyMODSS, WWdyMODAS, WWdyMODSA, WWdyMODAA, 
WLdyMODSS, WLdyMODAS, WLdyMODSA, WLdyMODAA, 
WSdyMODSS, WSdyMODAS, WSdyMODSA, WSdyMODAA, 
WDdyMODSS, WDdyMODAS, WDdyMODSA, WDdyMODAA, 
LBdyMODSS, LBdyMODAS, LBdyMODSA, LBdyMODAA, 
LWdyMODSS, LWdyMODAS, LWdyMODSA, LWdyMODAA, 
LLdyMODSS, LLdyMODAS, LLdyMODSA, LLdyMODAA, 
LSdyMODSS, LSdyMODAS, LSdyMODSA, LSdyMODAA, 
LDdyMODSS, LDdyMODAS, LDdyMODSA, LDdyMODAA, 
SBdyMODSS, SBdyMODAS, SBdyMODSA, SBdyMODAA, 
SWdyMODSS, SWdyMODAS, SWdyMODSA, SWdyMODAA, 
SLdyMODSS, SLdyMODAS, SLdyMODSA, SLdyMODAA, 
SSdyMODSS, SSdyMODAS, SSdyMODSA, SSdyMODAA, 
SDdyMODSS, SDdyMODAS, SDdyMODSA, SDdyMODAA, 
DBdyMODSS, DBdyMODAS, DBdyMODSA, DBdyMODAA, 
DWdyMODSS, DWdyMODAS, DWdyMODSA, DWdyMODAA, 
DLdyMODSS, DLdyMODAS, DLdyMODSA, DLdyMODAA, 
DSdyMODSS, DSdyMODAS, DSdyMODSA, DSdyMODAA, 
DDdyMODSS, DDdyMODAS, DDdyMODSA, DDdyMODAA, 

};
static dyadic_fct THEARClist[] = {
BBdyTHEARCSS, BBdyTHEARCAS, BBdyTHEARCSA, BBdyTHEARCAA, 
BWdyTHEARCSS, BWdyTHEARCAS, BWdyTHEARCSA, BWdyTHEARCAA, 
BLdyTHEARCSS, BLdyTHEARCAS, BLdyTHEARCSA, BLdyTHEARCAA, 
BSdyTHEARCSS, BSdyTHEARCAS, BSdyTHEARCSA, BSdyTHEARCAA, 
BDdyTHEARCSS, BDdyTHEARCAS, BDdyTHEARCSA, BDdyTHEARCAA, 
WBdyTHEARCSS, WBdyTHEARCAS, WBdyTHEARCSA, WBdyTHEARCAA, 
WWdyTHEARCSS, WWdyTHEARCAS, WWdyTHEARCSA, WWdyTHEARCAA, 
WLdyTHEARCSS, WLdyTHEARCAS, WLdyTHEARCSA, WLdyTHEARCAA, 
WSdyTHEARCSS, WSdyTHEARCAS, WSdyTHEARCSA, WSdyTHEARCAA, 
WDdyTHEARCSS, WDdyTHEARCAS, WDdyTHEARCSA, WDdyTHEARCAA, 
LBdyTHEARCSS, LBdyTHEARCAS, LBdyTHEARCSA, LBdyTHEARCAA, 
LWdyTHEARCSS, LWdyTHEARCAS, LWdyTHEARCSA, LWdyTHEARCAA, 
LLdyTHEARCSS, LLdyTHEARCAS, LLdyTHEARCSA, LLdyTHEARCAA, 
LSdyTHEARCSS, LSdyTHEARCAS, LSdyTHEARCSA, LSdyTHEARCAA, 
LDdyTHEARCSS, LDdyTHEARCAS, LDdyTHEARCSA, LDdyTHEARCAA, 
SBdyTHEARCSS, SBdyTHEARCAS, SBdyTHEARCSA, SBdyTHEARCAA, 
SWdyTHEARCSS, SWdyTHEARCAS, SWdyTHEARCSA, SWdyTHEARCAA, 
SLdyTHEARCSS, SLdyTHEARCAS, SLdyTHEARCSA, SLdyTHEARCAA, 
SSdyTHEARCSS, SSdyTHEARCAS, SSdyTHEARCSA, SSdyTHEARCAA, 
SDdyTHEARCSS, SDdyTHEARCAS, SDdyTHEARCSA, SDdyTHEARCAA, 
DBdyTHEARCSS, DBdyTHEARCAS, DBdyTHEARCSA, DBdyTHEARCAA, 
DWdyTHEARCSS, DWdyTHEARCAS, DWdyTHEARCSA, DWdyTHEARCAA, 
DLdyTHEARCSS, DLdyTHEARCAS, DLdyTHEARCSA, DLdyTHEARCAA, 
DSdyTHEARCSS, DSdyTHEARCAS, DSdyTHEARCSA, DSdyTHEARCAA, 
DDdyTHEARCSS, DDdyTHEARCAS, DDdyTHEARCSA, DDdyTHEARCAA, 

};

static void BmoNEGS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = -t;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoNEGA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = -t;
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoABSS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = fabs(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoABSA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = fabs(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoSQRTS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = sqrt(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoSQRTA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = sqrt(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoEXPS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = exp(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoEXPA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = exp(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoLNS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = log(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoLNA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = log(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoLGS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = log10(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoLGA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = log10(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoFLOORS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = floor(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoFLOORA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = floor(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoCEILS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = ceil(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoCEILA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = ceil(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoSINS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = sin(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoSINA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = sin(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoCOSS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = cos(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoCOSA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = cos(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoTANS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = tan(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoTANA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = tan(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoASINS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = asin(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoASINA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = asin(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoACOSS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = acos(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoACOSA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = acos(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void BmoATANS(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t = atan(t);
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void BmoATANA(B *df)
{
D t; B *d; L n;
d = (B *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((B *)d)) == BINF) t = DINF;
t = atan(t);
*((B *)d++) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}
}

static void WmoNEGS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = -t;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoNEGA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = -t;
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoABSS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = fabs(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoABSA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = fabs(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoSQRTS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = sqrt(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoSQRTA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = sqrt(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoEXPS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = exp(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoEXPA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = exp(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoLNS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = log(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoLNA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = log(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoLGS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = log10(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoLGA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = log10(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoFLOORS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = floor(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoFLOORA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = floor(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoCEILS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = ceil(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoCEILA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = ceil(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoSINS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = sin(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoSINA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = sin(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoCOSS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = cos(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoCOSA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = cos(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoTANS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = tan(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoTANA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = tan(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoASINS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = asin(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoASINA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = asin(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoACOSS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = acos(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoACOSA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = acos(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void WmoATANS(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t = atan(t);
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void WmoATANA(B *df)
{
D t; W *d; L n;
d = (W *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((W *)d)) == WINF) t = DINF;
t = atan(t);
*((W *)d++) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}
}

static void LmoNEGS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = -t;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoNEGA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = -t;
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoABSS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = fabs(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoABSA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = fabs(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoSQRTS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = sqrt(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoSQRTA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = sqrt(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoEXPS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = exp(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoEXPA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = exp(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoLNS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = log(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoLNA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = log(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoLGS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = log10(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoLGA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = log10(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoFLOORS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = floor(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoFLOORA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = floor(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoCEILS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = ceil(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoCEILA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = ceil(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoSINS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = sin(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoSINA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = sin(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoCOSS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = cos(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoCOSA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = cos(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoTANS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = tan(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoTANA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = tan(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoASINS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = asin(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoASINA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = asin(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoACOSS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = acos(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoACOSA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = acos(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void LmoATANS(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t = atan(t);
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void LmoATANA(B *df)
{
D t; L *d; L n;
d = (L *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
if ((t = *((L *)d)) == LINF) t = DINF;
t = atan(t);
*((L *)d++) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}
}

static void SmoNEGS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = -t;
*((S *)NUM_VAL(df)) = t;
}

static void SmoNEGA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = -t;
*((S *)d++) = t;
}
}

static void SmoABSS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = fabs(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoABSA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = fabs(t);
*((S *)d++) = t;
}
}

static void SmoSQRTS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = sqrt(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoSQRTA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = sqrt(t);
*((S *)d++) = t;
}
}

static void SmoEXPS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = exp(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoEXPA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = exp(t);
*((S *)d++) = t;
}
}

static void SmoLNS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = log(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoLNA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = log(t);
*((S *)d++) = t;
}
}

static void SmoLGS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = log10(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoLGA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = log10(t);
*((S *)d++) = t;
}
}

static void SmoFLOORS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = floor(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoFLOORA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = floor(t);
*((S *)d++) = t;
}
}

static void SmoCEILS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = ceil(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoCEILA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = ceil(t);
*((S *)d++) = t;
}
}

static void SmoSINS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = sin(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoSINA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = sin(t);
*((S *)d++) = t;
}
}

static void SmoCOSS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = cos(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoCOSA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = cos(t);
*((S *)d++) = t;
}
}

static void SmoTANS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = tan(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoTANA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = tan(t);
*((S *)d++) = t;
}
}

static void SmoASINS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = asin(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoASINA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = asin(t);
*((S *)d++) = t;
}
}

static void SmoACOSS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = acos(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoACOSA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = acos(t);
*((S *)d++) = t;
}
}

static void SmoATANS(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t = atan(t);
*((S *)NUM_VAL(df)) = t;
}

static void SmoATANA(B *df)
{
D t; S *d; L n;
d = (S *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((S *)d);
t = atan(t);
*((S *)d++) = t;
}
}

static void DmoNEGS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = -t;
*((D *)NUM_VAL(df)) = t;
}

static void DmoNEGA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = -t;
*((D *)d++) = t;
}
}

static void DmoABSS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = fabs(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoABSA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = fabs(t);
*((D *)d++) = t;
}
}

static void DmoSQRTS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = sqrt(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoSQRTA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = sqrt(t);
*((D *)d++) = t;
}
}

static void DmoEXPS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = exp(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoEXPA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = exp(t);
*((D *)d++) = t;
}
}

static void DmoLNS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = log(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoLNA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = log(t);
*((D *)d++) = t;
}
}

static void DmoLGS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = log10(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoLGA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = log10(t);
*((D *)d++) = t;
}
}

static void DmoFLOORS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = floor(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoFLOORA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = floor(t);
*((D *)d++) = t;
}
}

static void DmoCEILS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = ceil(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoCEILA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = ceil(t);
*((D *)d++) = t;
}
}

static void DmoSINS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = sin(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoSINA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = sin(t);
*((D *)d++) = t;
}
}

static void DmoCOSS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = cos(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoCOSA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = cos(t);
*((D *)d++) = t;
}
}

static void DmoTANS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = tan(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoTANA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = tan(t);
*((D *)d++) = t;
}
}

static void DmoASINS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = asin(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoASINA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = asin(t);
*((D *)d++) = t;
}
}

static void DmoACOSS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = acos(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoACOSA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = acos(t);
*((D *)d++) = t;
}
}

static void DmoATANS(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t = atan(t);
*((D *)NUM_VAL(df)) = t;
}

static void DmoATANA(B *df)
{
D t; D *d; L n;
d = (D *)VALUE_BASE(df);
for (n = ARRAY_SIZE(df); n>0; n--) {
t = *((D *)d);
t = atan(t);
*((D *)d++) = t;
}
}

typedef void (*monadic_fct)(B*);
static monadic_fct NEGlist[] = {
BmoNEGS, BmoNEGA, 
WmoNEGS, WmoNEGA, 
LmoNEGS, LmoNEGA, 
SmoNEGS, SmoNEGA, 
DmoNEGS, DmoNEGA, 

};
static monadic_fct ABSlist[] = {
BmoABSS, BmoABSA, 
WmoABSS, WmoABSA, 
LmoABSS, LmoABSA, 
SmoABSS, SmoABSA, 
DmoABSS, DmoABSA, 

};
static monadic_fct SQRTlist[] = {
BmoSQRTS, BmoSQRTA, 
WmoSQRTS, WmoSQRTA, 
LmoSQRTS, LmoSQRTA, 
SmoSQRTS, SmoSQRTA, 
DmoSQRTS, DmoSQRTA, 

};
static monadic_fct EXPlist[] = {
BmoEXPS, BmoEXPA, 
WmoEXPS, WmoEXPA, 
LmoEXPS, LmoEXPA, 
SmoEXPS, SmoEXPA, 
DmoEXPS, DmoEXPA, 

};
static monadic_fct LNlist[] = {
BmoLNS, BmoLNA, 
WmoLNS, WmoLNA, 
LmoLNS, LmoLNA, 
SmoLNS, SmoLNA, 
DmoLNS, DmoLNA, 

};
static monadic_fct LGlist[] = {
BmoLGS, BmoLGA, 
WmoLGS, WmoLGA, 
LmoLGS, LmoLGA, 
SmoLGS, SmoLGA, 
DmoLGS, DmoLGA, 

};
static monadic_fct FLOORlist[] = {
BmoFLOORS, BmoFLOORA, 
WmoFLOORS, WmoFLOORA, 
LmoFLOORS, LmoFLOORA, 
SmoFLOORS, SmoFLOORA, 
DmoFLOORS, DmoFLOORA, 

};
static monadic_fct CEILlist[] = {
BmoCEILS, BmoCEILA, 
WmoCEILS, WmoCEILA, 
LmoCEILS, LmoCEILA, 
SmoCEILS, SmoCEILA, 
DmoCEILS, DmoCEILA, 

};
static monadic_fct SINlist[] = {
BmoSINS, BmoSINA, 
WmoSINS, WmoSINA, 
LmoSINS, LmoSINA, 
SmoSINS, SmoSINA, 
DmoSINS, DmoSINA, 

};
static monadic_fct COSlist[] = {
BmoCOSS, BmoCOSA, 
WmoCOSS, WmoCOSA, 
LmoCOSS, LmoCOSA, 
SmoCOSS, SmoCOSA, 
DmoCOSS, DmoCOSA, 

};
static monadic_fct TANlist[] = {
BmoTANS, BmoTANA, 
WmoTANS, WmoTANA, 
LmoTANS, LmoTANA, 
SmoTANS, SmoTANA, 
DmoTANS, DmoTANA, 

};
static monadic_fct ASINlist[] = {
BmoASINS, BmoASINA, 
WmoASINS, WmoASINA, 
LmoASINS, LmoASINA, 
SmoASINS, SmoASINA, 
DmoASINS, DmoASINA, 

};
static monadic_fct ACOSlist[] = {
BmoACOSS, BmoACOSA, 
WmoACOSS, WmoACOSA, 
LmoACOSS, LmoACOSA, 
SmoACOSS, SmoACOSA, 
DmoACOSS, DmoACOSA, 

};
static monadic_fct ATANlist[] = {
BmoATANS, BmoATANA, 
WmoATANS, WmoATANA, 
LmoATANS, LmoATANA, 
SmoATANS, SmoATANA, 
DmoATANS, DmoATANA, 

};
static void Bdecr(B *df)
{
D t;
if ((t = *((B *)NUM_VAL(df))) == BINF) t = DINF;
t -= 1.0;
*((B *)NUM_VAL(df)) = (((t) > BMAX) || ((-t) < -BMAX) || (t == DINF))? BINF : t;
}

static void Wdecr(B *df)
{
D t;
if ((t = *((W *)NUM_VAL(df))) == WINF) t = DINF;
t -= 1.0;
*((W *)NUM_VAL(df)) = (((t) > WMAX) || ((-t) < -WMAX) || (t == DINF))? WINF : t;
}

static void Ldecr(B *df)
{
D t;
if ((t = *((L *)NUM_VAL(df))) == LINF) t = DINF;
t -= 1.0;
*((L *)NUM_VAL(df)) = (((t) > LMAX) || ((-t) < -LMAX) || (t == DINF))? LINF : t;
}

static void Sdecr(B *df)
{
D t;
t = *((S *)NUM_VAL(df));
t -= 1.0;
*((S *)NUM_VAL(df)) = t;
}

static void Ddecr(B *df)
{
D t;
t = *((D *)NUM_VAL(df));
t -= 1.0;
*((D *)NUM_VAL(df)) = t;
}

typedef void (*DECR_fct)(B*);
static DECR_fct DECRlist[] = {
Bdecr, Wdecr, Ldecr, Sdecr, Ddecr, 
};
