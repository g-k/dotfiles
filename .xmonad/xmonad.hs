import XMonad
import XMonad.Hooks.ManageDocks

main = xmonad $ defaultConfig {
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig
	}