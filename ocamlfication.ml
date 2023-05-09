
(* Equivalent code in ocmal

int foo(int x, int y, bool b) {
    if(x > y) {
        int t = x;
        x = y;
        y = t;
    }
    while(x < y) {
        if(b) {
            ++x;
        } else {
            --y;
        }
        b = !b;
    }
    return x;
} *)


let rec foo x y b =
    if x > y then
      let t = x in
      let x = y in
      let y = t in
      foo x y b
    else if x < y then
      let x, y =
        if b then
          x + 1, y
        else
          x, y - 1
      in
      foo x y (not b)
    else
      x