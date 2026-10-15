#### an area and each result are [x y w h]
### split a length into n parts separated by gap
(defn- strip
  [len n gap]
  (if (= n 0)
    @[]
    (let [unit (math/floor (/ (- len (* gap (dec n))) n))]
      (def out @[])
      (var used 0)
      (for i 0 n
        (def s (if (= i (dec n)) (- len used) unit))
        (array/push out s)
        (+= used (+ s gap)))
      out)))

### n areas side by side
(defn- hstack
  [[x y w h] n gap]
  (def out @[])
  (var cx x)
  (each cw (strip w n gap)
    (array/push out [cx y cw h])
    (+= cx (+ cw gap)))
  out)

### n areas stacked vertically
(defn- vstack
  [[x y w h] n gap]
  (def out @[])
  (var cy y)
  (each ch (strip h n gap)
    (array/push out [x cy w ch])
    (+= cy (+ ch gap)))
  out)

### split into [left right]
(defn- cut-v
  [[x y w h] ratio gap]
  (def avail (- w gap))
  (def lw (math/floor (* avail ratio)))
  [[x y lw h] [(+ x lw gap) y (- avail lw) h]])

### split into [top bottom]
(defn- cut-h
  [[x y w h] ratio gap]
  (def avail (- h gap))
  (def th (math/floor (* avail ratio)))
  [[x y w th] [x (+ y th gap) w (- avail th)]])

### master column left, stack right
(defn columns
  [area n opts]
  (def gap (get opts :gap 0))
  (def mc (min (get opts :master-count 1) n))
  (if (>= mc n)
    (vstack area n gap)
    (let [[m s] (cut-v area (get opts :master-factor 0.6) gap)]
      (array/concat (vstack m mc gap) (vstack s (- n mc) gap)))))

### master row top, stack below
(defn rows
  [area n opts]
  (def gap (get opts :gap 0))
  (def mc (min (get opts :master-count 1) n))
  (if (>= mc n)
    (hstack area n gap)
    (let [[m s] (cut-h area (get opts :master-factor 0.6) gap)]
      (array/concat (hstack m mc gap) (hstack s (- n mc) gap)))))

### roughly square grid
(defn grid
  [area n opts]
  (def gap (get opts :gap 0))
  (def nc (math/ceil (math/sqrt n)))
  (def nr (math/ceil (/ n nc)))
  (def out @[])
  (var left n)
  (each r (vstack area nr gap)
    (def k (min nc left))
    (array/concat out (hstack r k gap))
    (-= left k))
  out)

### recursive alternating splits
(defn dwindle
  [area n opts]
  (def gap (get opts :gap 0))
  (defn go [a k horiz?]
    (if (= k 1)
      @[a]
      (let [h (math/floor (/ k 2))
            [p q] (if horiz? (cut-v a 0.5 gap) (cut-h a 0.5 gap))]
        (array/concat (go p h (not horiz?)) (go q (- k h) (not horiz?))))))
  (go area n true))

### no tiling; the window manager places these itself
(defn floating
  [area n opts]
  @[])

(def layouts
  {"columns" columns "rows" rows "grid" grid
   "dwindle" dwindle "floating" floating})

(defn arrange
  [name area n opts]
  (def gap (get opts :gap 0))
  (def [x y w h] area)
  ((or (get layouts name) columns)
   [(+ x gap) (+ y gap) (max 1 (- w (* 2 gap))) (max 1 (- h (* 2 gap)))]
   n opts))
