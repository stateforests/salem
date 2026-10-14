(declare-project
  :name "salem"
  :description "A small window manager for the River compositor"
  :dependencies [{:url "https://codeberg.org/ifreund/janet-wayland"}])

(declare-executable
  :name "salem"
  :entry "src/salem.janet"
  :pkg-config-libs ["wayland-client"])
