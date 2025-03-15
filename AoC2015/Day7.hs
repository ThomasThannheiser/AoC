module Day7 where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Word

ls :: Word16
ls = lf .&. lq 
jn :: Word16
jn = iu `shiftR` 1 
bv :: Word16
bv = bo .|. bu 
hc :: Word16
hc = gj `shiftR` 1 
eu :: Word16
eu = et `shiftR` 2 
by :: Word16
by = bv .&. bx 
iu :: Word16
iu = is .|. it 
o :: Word16
o = b .|. n 
gg :: Word16
gg = gf .|. ge 
ku :: Word16
ku = complement kt 
ed :: Word16
ed = ea .&. eb 
ks :: Word16
ks = kl .|. kr 
hl :: Word16
hl = hi .&. hk 
ax :: Word16
ax = au .&. av 
lg :: Word16
lg = lf `shiftR` 2 
df :: Word16
df = dd `shiftR` 3 
fc :: Word16
fc = eu .&. fa 
di :: Word16
di = df .&. dg 
it :: Word16
it = ip `shiftL` 15 
em :: Word16
em = complement el 
ff :: Word16
ff = et .|. fe 
fn :: Word16
fn = fj `shiftL` 15 
u :: Word16
u = t .|. s 
ma :: Word16
ma = ly .|. lz 
kr :: Word16
kr = ko .&. kq 
fy :: Word16
fy = complement fx 
fm :: Word16
fm = et `shiftR` 1 
fb :: Word16
fb = eu .|. fa 
de :: Word16
de = dd `shiftR` 2 
gp :: Word16
gp = complement go 
ke :: Word16
ke = kb .&. kd 
hi :: Word16
hi = hg .|. hh 
kg :: Word16
kg = jm `shiftL` 1 
co :: Word16
co = complement cn 
jq :: Word16
jq = jp `shiftR` 2 
js :: Word16
js = jp `shiftR` 5 
ip :: Word16
ip = 1 .&. io 
es :: Word16
es = eo `shiftL` 15 
jk :: Word16
jk = 1 .&. jj 
j :: Word16
j = g .&. i 
ck :: Word16
ck = ci `shiftR` 3 
gq :: Word16
gq = gn .&. gp 
fv :: Word16
fv = fs .&. fu 
lm :: Word16
lm = lj .&. ll 
jo :: Word16
jo = jk `shiftL` 15 
iw :: Word16
iw = iu `shiftR` 3 
ij :: Word16
ij = complement ii 
cd :: Word16
cd = 1 .&. cc 
bp :: Word16
bp = bn `shiftR` 3 
gx :: Word16
gx = complement gw 
fu :: Word16
fu = complement ft 
jp :: Word16
jp = jn .|. jo 
jc :: Word16
jc = iv .|. jb 
hw :: Word16
hw = hv .|. hu 
b :: Word16
-- b = 19138        {-- part 1 --}
b = 16076           {-- part 2 
                        using the result of a from part 1 for b
                     --}
gm :: Word16
gm = gj `shiftR` 5 
ht :: Word16
ht = hq .&. hs 
er :: Word16
er = dy `shiftR` 1 
ap :: Word16
ap = ao .|. an 
lf :: Word16
lf = ld .|. le 
ce :: Word16
ce = bk `shiftL` 1 
cc :: Word16
cc = bz .&. cb 
bm :: Word16
bm = bi `shiftL` 15 
io :: Word16
io = il .&. _in 
ai :: Word16
ai = af .&. ah 
bl :: Word16
bl = as `shiftR` 1 
lh :: Word16
lh = lf `shiftR` 3 
et :: Word16
et = er .|. es 
ay :: Word16
ay = complement ax 
db :: Word16
db = ci `shiftR` 1 
fg :: Word16
fg = et .&. fe 
ln :: Word16
ln = lg .|. lm 
n :: Word16
n = k .&. m 
ia :: Word16
ia = hz `shiftR` 2 
lb :: Word16
lb = kh `shiftL` 1 
ez :: Word16
ez = complement ey 
dj :: Word16
dj = complement di 
eg :: Word16
eg = dz .|. ef 
a :: Word16
a = lx 
ja :: Word16
ja = complement iz 
hd :: Word16
hd = gz `shiftL` 15 
cf :: Word16
cf = ce .|. cd 
ft :: Word16
ft = fq .&. fr 
bb :: Word16
bb = at .&. az 
hb :: Word16
hb = ha .|. gz 
fx :: Word16
fx = fp .&. fv 
gc :: Word16
gc = complement gb 
ii :: Word16
ii = ia .&. ig 
gn :: Word16
gn = gl .|. gm 
c :: Word16
c = 0 
cb :: Word16
cb = complement ca 
cg :: Word16
cg = bn `shiftR` 1 
t :: Word16
t = c `shiftL` 1 
iy :: Word16
iy = iw .|. ix 
kh :: Word16
kh = kg .|. kf 
ek :: Word16
ek = dy .|. ej 
kp :: Word16
kp = km .&. kn 
fd :: Word16
fd = complement fc 
ib :: Word16
ib = hz `shiftR` 3 
dr :: Word16
dr = complement dq 
fh :: Word16
fh = complement fg 
dz :: Word16
dz = dy `shiftR` 2 
kl :: Word16
kl = kk `shiftR` 2 
fj :: Word16
fj = 1 .&. fi 
hs :: Word16
hs = complement hr 
ki :: Word16
ki = jp `shiftR` 1 
bn :: Word16
bn = bl .|. bm 
gz :: Word16
gz = 1 .&. gy 
gu :: Word16
gu = gr .&. gt 
dd :: Word16
dd = db .|. dc 
dl :: Word16
dl = de .|. dk 
av :: Word16
av = as `shiftR` 5 
li :: Word16
li = lf `shiftR` 5 
hp :: Word16
hp = hm .&. ho 
ci :: Word16
ci = cg .|. ch 
gw :: Word16
gw = gj .&. gu 
gi :: Word16
gi = ge `shiftL` 15 
g :: Word16
g = e .|. f 
fw :: Word16
fw = fp .|. fv 
fe :: Word16
fe = fb .&. fd 
ch :: Word16
ch = cd `shiftL` 15 
v :: Word16
v = b `shiftR` 1 
ba :: Word16
ba = at .|. az 
bo :: Word16
bo = bn `shiftR` 2 
lk :: Word16
lk = lh .&. li 
_do :: Word16
_do = dl .&. dn 
ej :: Word16
ej = eg .&. ei 
fa :: Word16
fa = ex .&. ez 
kq :: Word16
kq = complement kp 
ll :: Word16
ll = complement lk 
ak :: Word16
ak = x .&. ai 
kb :: Word16
kb = jp .|. ka 
je :: Word16
je = complement jd 
jb :: Word16
jb = iy .&. ja 
jr :: Word16
jr = jp `shiftR` 3 
ga :: Word16
ga = fo .|. fz 
dh :: Word16
dh = df .|. dg 
gk :: Word16
gk = gj `shiftR` 2 
gv :: Word16
gv = gj .|. gu 
ji :: Word16
ji = complement jh 
bj :: Word16
bj = ap `shiftL` 1 
lt :: Word16
lt = complement ls 
jl :: Word16
jl = ir `shiftL` 1 
ca :: Word16
ca = bn .&. by 
lz :: Word16
lz = lv `shiftL` 15 
bd :: Word16
bd = ba .&. bc 
dc :: Word16
dc = cy `shiftL` 15 
lq :: Word16
lq = ln .&. lp 
aq :: Word16
aq = x `shiftR` 1 
gr :: Word16
gr = gk .|. gq 
ky :: Word16
ky = complement kx 
jj :: Word16
jj = jg .&. ji 
bz :: Word16
bz = bn .|. by 
gf :: Word16
gf = fl `shiftL` 1 
br :: Word16
br = bp .|. bq 
hq :: Word16
hq = he .|. hp 
ew :: Word16
ew = et `shiftR` 5 
iv :: Word16
iv = iu `shiftR` 2 
go :: Word16
go = gl .&. gm 
aj :: Word16
aj = x .|. ai 
he :: Word16
he = hc .|. hd 
lo :: Word16
lo = lg .&. lm 
lj :: Word16
lj = lh .|. li 
du :: Word16
du = da `shiftL` 1 
fp :: Word16
fp = fo `shiftR` 2 
gs :: Word16
gs = gk .&. gq 
bk :: Word16
bk = bj .|. bi 
lr :: Word16
lr = lf .|. lq 
cr :: Word16
cr = cj .&. cp 
hy :: Word16
hy = hu `shiftL` 15 
bi :: Word16
bi = 1 .&. bh 
fq :: Word16
fq = fo `shiftR` 3 
lp :: Word16
lp = complement lo 
iq :: Word16
iq = hw `shiftL` 1 
dw :: Word16
dw = dd `shiftR` 1 
dx :: Word16
dx = dt `shiftL` 15 
el :: Word16
el = dy .&. ej 
ar :: Word16
ar = an `shiftL` 15 
as :: Word16
as = aq .|. ar 
s :: Word16
s = 1 .&. r 
fz :: Word16
fz = fw .&. fy 
_in :: Word16
_in = complement im 
ev :: Word16
ev = et `shiftR` 3 
dt :: Word16
dt = 1 .&. ds 
ef :: Word16
ef = ec .&. ee 
al :: Word16
al = complement ak 
jm :: Word16
jm = jl .|. jk 
eo :: Word16
eo = 1 .&. en 
lc :: Word16
lc = lb .|. la 
jh :: Word16
jh = iu .&. jf 
ix :: Word16
ix = iu `shiftR` 5 
bw :: Word16
bw = bo .&. bu 
da :: Word16
da = cz .|. cy 
jd :: Word16
jd = iv .&. jb 
iz :: Word16
iz = iw .&. ix 
ly :: Word16
ly = lf `shiftR` 1 
jg :: Word16
jg = iu .|. jf 
dn :: Word16
dn = complement dm 
lx :: Word16
lx = lw .|. lv 
ha :: Word16
ha = gg `shiftL` 1 
lu :: Word16
lu = lr .&. lt 
fo :: Word16
fo = fm .|. fn 
hg :: Word16
hg = he `shiftR` 3 
am :: Word16
am = aj .&. al 
la :: Word16
la = 1 .&. kz 
eb :: Word16
eb = dy `shiftR` 5 
jf :: Word16
jf = jc .&. je 
cp :: Word16
cp = cm .&. co 
gy :: Word16
gy = gv .&. gx 
ex :: Word16
ex = ev .|. ew 
kc :: Word16
kc = jp .&. ka 
fl :: Word16
fl = fk .|. fj 
ea :: Word16
ea = dy `shiftR` 3 
bt :: Word16
bt = complement bs 
ah :: Word16
ah = complement ag 
eh :: Word16
eh = dz .&. ef 
cz :: Word16
cz = cf `shiftL` 1 
cw :: Word16
cw = complement cv 
cy :: Word16
cy = 1 .&. cx 
dm :: Word16
dm = de .&. dk 
cn :: Word16
cn = ck .&. cl 
aa :: Word16
aa = x `shiftR` 5 
ep :: Word16
ep = dv `shiftL` 1 
hf :: Word16
hf = he `shiftR` 2 
bx :: Word16
bx = complement bw 
cm :: Word16
cm = ck .|. cl 
bs :: Word16
bs = bp .&. bq 
be :: Word16
be = as .|. bd 
hr :: Word16
hr = he .&. hp 
ey :: Word16
ey = ev .&. ew 
lv :: Word16
lv = 1 .&. lu 
km :: Word16
km = kk `shiftR` 3 
p :: Word16
p = b .&. n 
kd :: Word16
kd = complement kc 
lw :: Word16
lw = lc `shiftL` 1 
ko :: Word16
ko = km .|. kn 
ig :: Word16
ig = _id .&. _if 
ik :: Word16
ik = ih .&. ij 
ju :: Word16
ju = jr .&. js 
cl :: Word16
cl = ci `shiftR` 5 
is :: Word16
is = hz `shiftR` 1 
kf :: Word16
kf = 1 .&. ke 
gt :: Word16
gt = complement gs 
az :: Word16
az = aw .&. ay 
y :: Word16
y = x `shiftR` 2 
ae :: Word16
ae = ab .&. ad 
fi :: Word16
fi = ff .&. fh 
cv :: Word16
cv = ci .&. ct 
fk :: Word16
fk = eq `shiftL` 1 
gl :: Word16
gl = gj `shiftR` 3 
ao :: Word16
ao = u `shiftL` 1 
bc :: Word16
bc = complement bb 
hk :: Word16
hk = complement hj 
kz :: Word16
kz = kw .&. ky 
bf :: Word16
bf = as .&. bd 
dy :: Word16
dy = dw .|. dx 
bu :: Word16
bu = br .&. bt 
kx :: Word16
kx = kk .&. kv 
eq :: Word16
eq = ep .|. eo 
hx :: Word16
hx = he `shiftR` 1 
kk :: Word16
kk = ki .|. kj 
jv :: Word16
jv = complement ju 
en :: Word16
en = ek .&. em 
kn :: Word16
kn = kk `shiftR` 5 
ei :: Word16
ei = complement eh 
hz :: Word16
hz = hx .|. hy 
ec :: Word16
ec = ea .|. eb 
w :: Word16
w = s `shiftL` 15 
gh :: Word16
gh = fo `shiftR` 1 
kw :: Word16
kw = kk .|. kv 
bq :: Word16
bq = bn `shiftR` 5 
ee :: Word16
ee = complement ed 
hu :: Word16
hu = 1 .&. ht 
cx :: Word16
cx = cu .&. cw 
f :: Word16
f = b `shiftR` 5 
kt :: Word16
kt = kl .&. kr 
ir :: Word16
ir = iq .|. ip 
cj :: Word16
cj = ci `shiftR` 2 
cq :: Word16
cq = cj .|. cp 
r :: Word16
r = o .&. q 
dg :: Word16
dg = dd `shiftR` 5 
d :: Word16
d = b `shiftR` 2 
kv :: Word16
kv = ks .&. ku 
e :: Word16
e = b `shiftR` 3 
k :: Word16
k = d .|. j 
q :: Word16
q = complement p 
cs :: Word16
cs = complement cr 
dv :: Word16
dv = du .|. dt 
kj :: Word16
kj = kf `shiftL` 15 
ad :: Word16
ad = complement ac 
fr :: Word16
fr = fo `shiftR` 5 
il :: Word16
il = hz .|. ik 
ka :: Word16
ka = jx .&. jz 
gj :: Word16
gj = gh .|. gi 
ld :: Word16
ld = kk `shiftR` 1 
ic :: Word16
ic = hz `shiftR` 5 
at :: Word16
at = as `shiftR` 2 
jz :: Word16
jz = complement jy 
an :: Word16
an = 1 .&. am 
cu :: Word16
cu = ci .|. ct 
hj :: Word16
hj = hg .&. hh 
jx :: Word16
jx = jq .|. jw 
x :: Word16
x = v .|. w 
le :: Word16
le = la `shiftL` 15 
dk :: Word16
dk = dh .&. dj 
ds :: Word16
ds = dp .&. dr 
jy :: Word16
jy = jq .&. jw 
aw :: Word16
aw = au .|. av 
bg :: Word16
bg = complement bf 
ab :: Word16
ab = z .|. aa 
gd :: Word16
gd = ga .&. gc 
im :: Word16
im = hz .&. ik 
jw :: Word16
jw = jt .&. jv 
ac :: Word16
ac = z .&. aa 
jt :: Word16
jt = jr .|. js 
hv :: Word16
hv = hb `shiftL` 1 
hm :: Word16
hm = hf .|. hl 
_id :: Word16
_id = ib .|. ic 
fs :: Word16
fs = fq .|. fr 
ct :: Word16
ct = cq .&. cs 
ih :: Word16
ih = ia .|. ig 
dp :: Word16
dp = dd .|. _do 
l :: Word16
l = d .&. j 
ie :: Word16
ie = ib .&. ic 
au :: Word16
au = as `shiftR` 3 
bh :: Word16
bh = be .&. bg 
dq :: Word16
dq = dd .&. _do 
m :: Word16
m = complement l 
ge :: Word16
ge = 1 .&. gd 
ag :: Word16
ag = y .&. ae 
gb :: Word16
gb = fo .&. fz 
_if :: Word16
_if = complement ie 
h :: Word16
h = e .&. f 
z :: Word16
z = x `shiftR` 3 
af :: Word16
af = y .|. ae 
hn :: Word16
hn = hf .&. hl 
i :: Word16
i = complement h 
ho :: Word16
ho = complement hn 
hh :: Word16
hh = he `shiftR` 5

-- 16076
-- 2797

main :: IO ()
main = print a