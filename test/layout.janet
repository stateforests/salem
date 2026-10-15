#### run: JANET_PATH=src janet test/layout.janet
(import layout :as l)

(var failures 0)
(defn check [name ok]
  (if ok
    (print "ok   " name)
    (do (++ failures) (print "FAIL " name))))

(def area [0 0 100 100])
(def plain {:gap 0 :master-factor 0.5 :master-count 1})

(check "columns n=3 -> 3 rects" (= 3 (length (l/arrange "columns" area 3 plain))))
(check "rows n=3 -> 3 rects" (= 3 (length (l/arrange "rows" area 3 plain))))
(check "grid n=5 -> 5 rects" (= 5 (length (l/arrange "grid" area 5 plain))))
(check "dwindle n=4 -> 4 rects" (= 4 (length (l/arrange "dwindle" area 4 plain))))
(check "columns n=0 -> 0 rects" (= 0 (length (l/arrange "columns" area 0 plain))))
(check "floating tiles nothing" (= 0 (length (l/arrange "floating" area 3 plain))))
(check "unknown layout falls back to columns"
       (= 2 (length (l/arrange "nonsense" area 2 plain))))

### master-stack
(def cols (l/arrange "columns" area 3 plain))
(check "columns master is left half" (= [0 0 50 100] (cols 0)))
(check "columns stack is right half" (= [50 0 50 50] (cols 1)))
(check "columns stack second slot" (= [50 50 50 50] (cols 2)))

### rows transposition
(def rws (l/arrange "rows" area 3 plain))
(check "rows master is top half" (= [0 0 100 50] (rws 0)))
(check "rows stack first slot" (= [0 50 50 50] (rws 1)))

### grid
(def g (l/arrange "grid" area 4 plain))
(check "grid first cell" (= [0 0 50 50] (g 0)))
(check "grid last cell" (= [50 50 50 50] (g 3)))

### dwindle
(def d (l/arrange "dwindle" area 4 plain))
(check "dwindle first quadrant" (= [0 0 50 50] (d 0)))
(check "dwindle second quadrant" (= [0 50 50 50] (d 1)))
(check "dwindle third quadrant" (= [50 0 50 50] (d 2)))
(check "dwindle fourth quadrant" (= [50 50 50 50] (d 3)))

### gaps
(def gp (l/arrange "columns" area 2 {:gap 10 :master-factor 0.5 :master-count 1}))
(check "gap insets the area" (= [10 10 35 80] (gp 0)))
(check "gap separates tiles" (= [55 10 35 80] (gp 1)))

(if (> failures 0)
  (do (printf "%d layout tests failed" failures) (os/exit 1))
  (print "all layout tests passed"))
