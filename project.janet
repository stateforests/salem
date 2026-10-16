(declare-project
  :name "salem"
  :description "A small window manager for the River compositor"
  :dependencies [{:url "https://codeberg.org/ifreund/janet-wayland"}])

(declare-executable
  :name "salem"
  :entry "src/salem.janet"
  :deps ["src/wm.janet" "src/layout.janet" "src/config.janet" "src/keysym.janet"]
  :pkg-config-libs ["wayland-client"])
