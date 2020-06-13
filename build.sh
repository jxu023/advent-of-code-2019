
# this helps me view the error log from the top when it comes out...
# stops me from scrolling every time.

# consider replacing the 'ghc day2.hs' with arguments ..
ghc -o a.out -XDeriveAnyClass $1.hs -g 2>log

# if file log is size > 0, then open it w/ vi
if [[ -s log ]]
then
    vi log
else
    ./$1 # < $1.input
fi
