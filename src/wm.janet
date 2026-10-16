(import ./layout)
(import ./keysym)
(import ./config)

(def state
  @{:wm nil :bindings nil :config nil
    :windows @[] :outputs @[] :seats @[]
    :focused nil :focus-output nil})

(def all-edges {:top true :bottom true :left true :right true})
(def no-edges {})
(def caps {:fullscreen true})

(var actions @{})

(defn- conf [k] (get (state :config) k))
(defn- color32 [v] (* v 0x01010101))
(defn- log [msg] (eprint (string "salem: " msg)))

### queries
(defn- focus-output []
  (or (state :focus-output) (first (state :outputs))))

(defn- out-layout [out]
  (or (get (out :layouts) (out :current)) (conf :layout)))

(defn- layout-floating? [out]
  (= "floating" (out-layout out)))

(defn- visible [out]
  (filter |(and (= ($ :output) out) (= ($ :workspace) (out :current)))
          (state :windows)))

(defn- window-visible? [w]
  (def out (w :output))
  (and out (= (w :workspace) (out :current))))

(defn- window-tiled? [w]
  (and (window-visible? w)
       (not (w :float))
       (not (w :fullscreen))
       (not (layout-floating? (w :output)))))

(defn- tiled [out]
  (filter window-tiled? (visible out)))

### pair each tiled window of an output with its [x y w h] rect
(defn- placement [out]
  (def ts (tiled out))
  (def rects (layout/arrange (out-layout out)
                             [(out :x) (out :y) (out :w) (out :h)]
                             (length ts)
                             {:gap (conf :gap)
                              :master-factor (conf :master-factor)
                              :master-count (conf :master-count)}))
  (map (fn [w r] [w r]) ts rects))

### focus
(defn- fix-focus []
  (def out (focus-output))
  (def vis (if out (visible out) @[]))
  (def cur (state :focused))
  (unless (and cur (some |(= $ cur) vis))
    (put state :focused (if (empty? vis) nil (first vis)))))

(defn- cycle-focus [dir]
  (def out (focus-output))
  (when out
    (def vis (visible out))
    (when (> (length vis) 1)
      (def i (or (index-of (state :focused) vis) 0))
      (put state :focused (vis (mod (+ i dir) (length vis)))))))

(defn- focus-output-next []
  (def outs (state :outputs))
  (when (> (length outs) 1)
    (def i (or (index-of (focus-output) outs) 0))
    (put state :focus-output (outs (mod (inc i) (length outs))))
    (fix-focus)))

(defn- move-output-next []
  (def w (state :focused))
  (def outs (state :outputs))
  (when (and w (> (length outs) 1))
    (def i (or (index-of (w :output) outs) 0))
    (def o (outs (mod (inc i) (length outs))))
    (put w :output o)
    (put w :workspace (o :current))
    (put w :x nil)
    (put w :y nil)
    (fix-focus)))

(defn- focus-window [prox]
  (def w (:get-user-data prox))
  (when w
    (put state :focused w)
    (when (w :output)
      (put state :focus-output (w :output))
      (put (w :output) :current (w :workspace)))))

### commands
(defn- close-focused []
  (def w (state :focused))
  (when w (put w :closing true)))

(defn- move-window [dir]
  (def w (state :focused))
  (when w
    (def peers (filter |(and (= ($ :output) (w :output))
                             (= ($ :workspace) (w :workspace)))
                       (state :windows)))
    (def i (index-of w peers))
    (when i
      (def j (+ i dir))
      (when (and (>= j 0) (< j (length peers)))
        (def other (peers j))
        (def arr (state :windows))
        (def ia (index-of w arr))
        (def ib (index-of other arr))
        (put arr ia other)
        (put arr ib w)))))

(defn- switch-workspace [n]
  (def out (focus-output))
  (when out
    (put out :current n)
    (fix-focus)))

(defn- send-to-workspace [n]
  (def w (state :focused))
  (when w
    (put w :workspace n)
    (fix-focus)))

(defn- set-layout [name]
  (def out (focus-output))
  (when out (put (out :layouts) (out :current) name)))

(defn- layout-next []
  (def out (focus-output))
  (when out
    (def cyc (conf :layout-cycle))
    (def i (index-of (out-layout out) cyc))
    (set-layout (cyc (mod (inc (or i -1)) (length cyc))))))

(defn- toggle-float []
  (def w (state :focused))
  (when w
    (put w :float (not (w :float)))
    (when (w :float)
      (put w :x nil)
      (put w :y nil))))

(defn- toggle-fullscreen []
  (def w (state :focused))
  (when w
    (if (w :fullscreen)
      (do (put w :fullscreen false) (put w :need-dims true))
      (do (put w :fullscreen true)
          (put w :fs-output (or (w :fs-output) (w :output)))))))

(defn- spawn [cmd]
  (try
    ### background via sh so the child reparents to init and we never reap it
    (os/proc-wait (os/spawn ["/bin/sh" "-c" (string cmd " &")] :p))
    ([err] (log (string "spawn failed: " err)))))

(defn- run-action [act]
  (cond
    (function? act) (act)
    (get actions act) ((get actions act))
    (log (string "unknown action " act))))

(defn- binding-handler [act]
  (fn [event]
    (match event
      [:pressed] (run-action act)
      _ nil)))

(defn- init-actions []
  (set actions
    @{:close close-focused
      :focus-next (fn [] (cycle-focus 1))
      :focus-prev (fn [] (cycle-focus -1))
      :focus-output-next focus-output-next
      :move-output-next move-output-next
      :move-next (fn [] (move-window 1))
      :move-prev (fn [] (move-window -1))
      :toggle-float toggle-float
      :toggle-fullscreen toggle-fullscreen
      :layout-next layout-next
      :layout-columns (fn [] (set-layout "columns"))
      :layout-rows (fn [] (set-layout "rows"))
      :layout-grid (fn [] (set-layout "grid"))
      :layout-dwindle (fn [] (set-layout "dwindle"))
      :layout-floating (fn [] (set-layout "floating"))
      :spawn-terminal (fn [] (spawn (conf :terminal)))})
  (for i 0 (conf :workspaces)
    (def n (inc i))
    (put actions (keyword (string "ws-" n)) (let [n n] (fn [] (switch-workspace n))))
    (put actions (keyword (string "move-ws-" n)) (let [n n] (fn [] (send-to-workspace n))))))

### events
(defn- apply-rules [w]
  (when (and (not (w :ruled)) (w :app-id))
    (each rule (conf :rules)
      (when (config/rule-matches? rule w)
        (when (rule :float) (put w :float true))
        (when (rule :workspace) (put w :workspace (rule :workspace)))
        (when (rule :layout)
          (def out (w :output))
          (when out (put (out :layouts) (w :workspace) (rule :layout))))))
    (put w :ruled true)))

(defn- ensure-bindings []
  (when-let [bg (state :bindings)]
    (each s (state :seats)
      (unless (s :ready)
        (put s :ready true)
        (each [keyname mods act] (conf :bindings)
          (def b (:get-xkb-binding bg (s :proxy) (keysym/value keyname) mods))
          (:set-handler b (binding-handler act))
          (array/push (s :bindings) b))
        (each b (s :bindings)
          (:enable b))))))

(defn- manage-start []
  (each w (state :windows) (apply-rules w))
  (ensure-bindings)

  (def out (focus-output))
  (each w (state :windows)
    (when (nil? (w :output)) (put w :output out)))
  (fix-focus)

  (def seat (first (state :seats)))
  (each w (state :windows)
    (when (w :output)
      (unless (w :node)
        (put w :node (:get-node (w :proxy))))
      (unless (w :caps)
        (:set-capabilities (w :proxy) caps)
        (put w :caps true))
      (cond
        (and (w :fullscreen) (not (w :fs-set)))
        (do (:fullscreen (w :proxy) ((or (w :fs-output) (w :output)) :proxy))
            (:inform-fullscreen (w :proxy))
            (put w :fs-set true))
        (and (not (w :fullscreen)) (w :fs-set))
        (do (:exit-fullscreen (w :proxy))
            (:inform-not-fullscreen (w :proxy))
            (put w :fs-set false)
            (put w :need-dims true)))
      (:set-tiled (w :proxy) (if (window-tiled? w) all-edges no-edges))
      (when (w :closing)
        (:close (w :proxy))
        (put w :closing false))))

  (each o (state :outputs)
    (when (> (o :w) 0)
      (each [w r] (placement o)
        (:propose-dimensions (w :proxy) (r 2) (r 3)))
      (each w (visible o)
        (when (and (not (window-tiled? w)) (not (w :fullscreen))
                   (or (w :need-dims) (= 0 (w :w))))
          (:propose-dimensions (w :proxy) 0 0)
          (put w :need-dims false)))))

  (when seat
    (def w (state :focused))
    (if w
      (:focus-window (seat :proxy) (w :proxy))
      (:clear-focus (seat :proxy))))
  (:manage-finish (state :wm)))

(defn- render-start []
  (def bw (conf :border-width))
  (each o (state :outputs)
    (when (> (o :w) 0)
      (each [w r] (placement o)
        (put w :x (r 0))
        (put w :y (r 1)))
      (each w (visible o)
        (when (and (not (window-tiled? w)) (not (w :fullscreen)) (nil? (w :x)))
          (def n (get state :cascade 0))
          (put state :cascade (mod (inc n) 12))
          (put w :x (+ (o :x) 48 (* 32 n)))
          (put w :y (+ (o :y) 48 (* 32 n)))))))

  (def focus (state :focused))
  (each w (state :windows)
    (if (window-visible? w)
      (do
        (when (w :hidden) (:show (w :proxy)) (put w :hidden false))
        (when (and (w :node) (not (w :fullscreen)))
          (:set-position (w :node) (or (w :x) 0) (or (w :y) 0)))
        (when (> bw 0)
          (def c (if (= w focus) (conf :border-focus) (conf :border-normal)))
          (:set-borders (w :proxy) all-edges bw
                        (color32 (c 0)) (color32 (c 1)) (color32 (c 2)) (color32 (c 3)))))
      (unless (w :hidden)
        (:hide (w :proxy))
        (put w :hidden true))))

  (when (and focus (focus :node) (not (focus :fullscreen)))
    (:place-top (focus :node)))
  (:render-finish (state :wm)))

(defn- close-window [w]
  (def arr (state :windows))
  (array/remove arr (index-of w arr))
  (when (w :node) (:destroy (w :node)))
  (:destroy (w :proxy))
  (when (= w (state :focused)) (put state :focused nil))
  (fix-focus))

(defn- window-event [w event]
  (match event
    [:closed] (close-window w)
    [:dimensions width height] (do (put w :w width) (put w :h height))
    [:app-id id] (put w :app-id id)
    [:title t] (put w :title t)
    [:parent p] (put w :parent p)
    [:dimensions-hint mnw mnh mxw mxh]
    (do (put w :min-w mnw) (put w :min-h mnh) (put w :max-w mxw) (put w :max-h mxh))
    [:decoration-hint hint] (put w :decoration hint)
    [:fullscreen-requested o]
    (do (put w :fullscreen true) (when o (put w :fs-output (:get-user-data o))))
    [:exit-fullscreen-requested] (do (put w :fullscreen false) (put w :need-dims true))
    _ nil))

(defn- add-window [prox]
  (def w @{:proxy prox :node nil :app-id nil :title nil :parent nil
           :output nil :workspace 1 :float false :fullscreen false
           :fs-output nil :fs-set false :closing false :ruled false :caps false
           :hidden false :need-dims true :decoration 3
           :x nil :y nil :w 0 :h 0 :min-w 0 :min-h 0 :max-w 0 :max-h 0})
  (def out (focus-output))
  (when out
    (put w :output out)
    (put w :workspace (out :current)))
  (array/push (state :windows) w)
  (:set-user-data prox w)
  (:set-handler prox (fn [event] (window-event w event)))
  (put state :focused w))

(defn- remove-output [o]
  (def outs (state :outputs))
  (array/remove outs (index-of o outs))
  (def other (first outs))
  (each w (state :windows)
    (when (= (w :output) o) (put w :output other)))
  (when (= (state :focus-output) o) (put state :focus-output other))
  (:destroy (o :proxy)))

(defn- output-event [o event]
  (match event
    [:removed] (remove-output o)
    [:position x y] (do (put o :x x) (put o :y y))
    [:dimensions w h] (do (put o :w w) (put o :h h))
    _ nil))

(defn- add-output [prox]
  (def o @{:proxy prox :x 0 :y 0 :w 0 :h 0 :current 1 :layouts @{}})
  (array/push (state :outputs) o)
  (:set-user-data prox o)
  (:set-handler prox (fn [event] (output-event o event)))
  (unless (state :focus-output) (put state :focus-output o))
  o)

(defn- remove-seat [s]
  (array/remove (state :seats) (index-of s (state :seats)))
  (:destroy (s :proxy)))

(defn- seat-event [s event]
  (match event
    [:removed] (remove-seat s)
    [:window-interaction w] (focus-window w)
    _ nil))

(defn- add-seat [prox]
  (def s @{:proxy prox :bindings @[] :ready false})
  (array/push (state :seats) s)
  (:set-handler prox (fn [event] (seat-event s event)))
  s)

(defn handle
  [event]
  (match event
    [:unavailable] (do (log "another window manager is running") (os/exit 1))
    [:finished] (os/exit 0)
    [:manage-start] (manage-start)
    [:render-start] (render-start)
    [:window w] (add-window w)
    [:output o] (add-output o)
    [:seat s] (add-seat s)
    _ nil))

### entry
(defn init [config]
  (put state :config config)
  (init-actions))

(defn attach-wm [prox]
  (put state :wm prox)
  (:set-handler prox handle))

(defn attach-bindings [prox]
  (put state :bindings prox))

(defn ready? []
  (truthy? (state :wm)))
