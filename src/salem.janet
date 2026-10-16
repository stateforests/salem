(import wayland)
(import ./config)
(import ./wm)

### vendored river protocol xml
(defn- dirname [path]
  (string/join (slice (string/split "/" path) 0 -2) "/"))

(defn- proto [name]
  (string (dirname (dyn :current-file)) "/../protocols/" name ".xml"))

(def interfaces
  (wayland/scan
    :system-protocols []
    :custom-protocols (map proto ["river-window-management-v1"
                                  "river-xkb-bindings-v1"])))

(var registry nil)

(defn- attach-global [name interface version]
  (case interface
    "river_window_manager_v1" (wm/attach-wm (:bind registry name interface (min version 5)))
    "river_xkb_bindings_v1" (wm/attach-bindings (:bind registry name interface (min version 3)))
    nil))

(defn- registry-handle [event]
  (match event
    [:global name interface version]
    (attach-global name interface version)
    _ nil))

(defn main [& args]
  (wm/init (config/load))
  (def display (wayland/connect interfaces))

  (set registry (:get-registry display))
  (:set-handler registry registry-handle)
  (:roundtrip display)

  (unless (wm/ready?)
    (error "river_window_manager_v1 not exposed"))

  (forever (:dispatch display)))
