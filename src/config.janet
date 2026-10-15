(defn- workspace-bindings []
  (def shifted ["exclam" "at" "numbersign" "dollar" "percent"
                "asciicircum" "ampersand" "asterisk" "parenleft"])
  (def out @[])
  (for i 0 9
    (def n (inc i))
    (array/push out [(string n) {:mod4 true} (keyword (string "ws-" n))])
    (array/push out [(shifted i) {:mod4 true :shift true} (keyword (string "move-ws-" n))]))
  out)

(def base-bindings
  [["Return" {:mod4 true} :spawn-terminal]
   ["q" {:mod4 true} :close]
   ["j" {:mod4 true} :focus-next]
   ["k" {:mod4 true} :focus-prev]
   ["Left" {:mod4 true} :focus-prev]
   ["Right" {:mod4 true} :focus-next]
   ["J" {:mod4 true :shift true} :move-next]
   ["K" {:mod4 true :shift true} :move-prev]
   ["f" {:mod4 true} :toggle-float]
   ["F" {:mod4 true :shift true} :toggle-fullscreen]
   ["space" {:mod4 true} :layout-next]
   ["comma" {:mod4 true} :layout-columns]
   ["period" {:mod4 true} :layout-dwindle]
   ["o" {:mod4 true} :focus-output-next]
   ["O" {:mod4 true :shift true} :move-output-next]])

### ~/.config/salem/init.janet
(def defaults
  (let [t @{:terminal "foot"
            :gap 8
            :border-width 2
            :border-focus [0x66 0xaa 0xff 0xff]
            :border-normal [0x2a 0x2a 0x2a 0xff]
            :master-factor 0.6
            :master-count 1
            :layout "columns"
            :layout-cycle ["columns" "rows" "grid" "dwindle" "floating"]
            :workspaces 9
            :rules []
            :bindings []}]
    (put t :bindings (array/concat (array ;base-bindings) (workspace-bindings)))
    t))

(defn rule-matches?
  [rule w]
  (and (or (nil? (rule :app-id)) (= (rule :app-id) (w :app-id)))
       (or (nil? (rule :title)) (= (rule :title) (w :title)))))

(defn load
  [&opt path]
  (def base (table/clone defaults))
  (def path (or path
                (string (os/getenv "XDG_CONFIG_HOME"
                                   (string (os/getenv "HOME" ".") "/.config"))
                        "/salem/init.janet")))
  (when (os/stat path)
    (def user (dofile path))
    (when user (merge-into base user)))
  base)
