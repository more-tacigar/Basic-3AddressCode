# Basic-3AddressCode - 3 Address Code Example - #

３番地コード。

```
local a = 1;
func f(b) {
  if (n == 1) {
    local d = b + a;
  }
  local c = b;
}
func main() {
  local i = 0;
  for (i = 0; i < 10; i = i + 1;) {
    f(i);
  }
}
```

will be translated into 

```
__tmp1 = 1
a = __tmp1
func begin f
__tmp2 = n
__tmp3 = 1
__tmp4 = __tmp2 == __tmp3
if __tmp4 goto L1
__tmp5 = b
__tmp6 = a
__tmp7 = __tmp5 + __tmp6
d = __tmp7
L1: __tmp8 = b
c = __tmp8
func end
func begin main
__tmp9 = 0
i = __tmp9
__tmp10 = 0
i = __tmp10
L2: __tmp11 = i
__tmp12 = 10
__tmp13 = __tmp11 < __tmp12
if __tmp13 goto L3
__tmp14 = i
__tmp15 = 1
__tmp16 = __tmp14 + __tmp15
i = __tmp16
__tmp17 = i
param __tmp17
call f1
goto L2
L3: func end
```
