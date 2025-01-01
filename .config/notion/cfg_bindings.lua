--
-- Notion core configuration file
--


--
-- Bindings. This includes global bindings and bindings common to
-- screens and all types of frames only. See modules' configuration
-- files for other bindings.
--


-- WScreen context bindings
--
-- The bindings in this context are available all the time.
--
-- The variable META should contain a string of the form 'Mod4+'
-- where Mod4 maybe replaced with the modifier you want to use for most
-- of the bindings. Similarly ALTMETA may be redefined to add a
-- modifier to some of the F-key bindings.

defbindings("WScreen", {
    bdoc("Switch to object 0 (workspace, full screen client window) "..
         "within current screen.", "ws 0"),
    kpress(META.."1", "WScreen.switch_nth(_, 0)"),
    bdoc("Switch to object 1 (workspace, full screen client window) "..
         "within current screen.", "ws 1"),
    kpress(META.."2", "WScreen.switch_nth(_, 1)"),
    bdoc("Switch to object 2 (workspace, full screen client window) "..
         "within current screen.", "ws 2"),
    kpress(META.."3", "WScreen.switch_nth(_, 2)"),
    bdoc("Switch to object 3 (workspace, full screen client window) "..
         "within current screen.", "ws 3"),
    kpress(META.."4", "WScreen.switch_nth(_, 3)"),
    bdoc("Switch to object 4 (workspace, full screen client window) "..
         "within current screen.", "ws 4"),
    kpress(META.."5", "WScreen.switch_nth(_, 4)"),
    bdoc("Switch to object 5 (workspace, full screen client window) "..
         "within current screen.", "ws 5"),
    kpress(META.."6", "WScreen.switch_nth(_, 5)"),
    bdoc("Switch to object 6 (workspace, full screen client window) "..
         "within current screen.", "ws 6"),
    kpress(META.."7", "WScreen.switch_nth(_, 6)"),
    bdoc("Switch to object 7 (workspace, full screen client window) "..
         "within current screen.", "ws 7"),
    kpress(META.."8", "WScreen.switch_nth(_, 7)"),
    bdoc("Switch to object 8 (workspace, full screen client window) "..
         "within current screen.", "ws 8"),
    kpress(META.."9", "WScreen.switch_nth(_, 8)"),
    bdoc("Switch to object 9 (workspace, full screen client window) "..
         "within current screen.", "ws 9"),

    bdoc("Display the window list menu."),
    mpress("Button2", "mod_menu.pmenu(_, _sub, 'windowlist')"),

	-- '_chld' used here stands to for an actual child window that may not
    -- be managed by the screen itself, unlike '_sub', that is likely to be
    -- the managing group of that window. The right/left directions are
    -- used instead of next/prev, because they work better in conjunction
    -- with tilings.
    bdoc("Forward-circulate focus.", "->frame"),
    kpress(META.."Tab", "ioncore.goto_next(_chld, 'right')",
           "_chld:non-nil"),

    bdoc("Backward-circulate focus.", "<-frame"),
    kpress(META.."Shift+Tab", "ioncore.goto_next(_chld, 'left')",
           "_chld:non-nil"),

	bdoc("Forward-circulate focus.", "<up_arrow> frame"),
    kpress(ALTMETA.."Tab", "ioncore.goto_next(_chld, 'top')",
           "_chld:non-nil"),

    bdoc("Backward-circulate focus.", "<bottom_arrow> frame"),
    kpress(ALTMETA.."Shift+Tab", "ioncore.goto_next(_chld, 'bottom')",
           "_chld:non-nil"),

	bdoc("Start new Workspace", "create_ws"),
	kpress(META.."Shift+n", "ioncore.create_ws(_)"),

	bdoc("Go to previous Workspace", "previous_ws"),
	kpress(META.."Shift+p", "ioncore.goto_previous_workspace()"),

	bdoc("Move workspace to the right", "right_ws"),
    kpress(META.."Shift+m", "WScreen.inc_index(_, _sub)", "_sub:non-nil"),

	bdoc("Move workspace to the left", "left_ws"),
    kpress(META.."Shift+z", "WScreen.dec_index(_, _sub)", "_sub:non-nil"),

	
    bdoc("Raise focused object, if possible.", "raise"),
    kpress(ALTMETA.."R", "WRegion.rqorder(_chld, 'front')",
           "_chld:non-nil"),
})

-- WGroupWS
defbindings("WGroupWS", {
	bdoc("Rename current Workspace", "rename_ws"),
	kpress(META.."Shift+w", "mod_query.query_renameworkspace(nil, _)"),
})

-- Client window bindings
--
-- These bindings affect client windows directly.

defbindings("WClientWin", {
    bdoc("Nudge the client window. This might help with some "..
      "programs' resizing problems.", "nudge"),
    kpress_wait(ALTMETA.."N", "WClientWin.nudge(_)"),

    bdoc("Kill client owning the client window.", "kill"),
    kpress(META.."Shift+c", "WClientWin.kill(_)"),

})


-- Client window group bindings

defbindings("WGroupCW", {
    bdoc("Toggle client window group full-screen mode", "fullscr"),
    kpress_wait(ALTMETA.."f", "WGroup.set_fullscreen(_, 'toggle')"),
})


-- WMPlex context bindings
--
-- These bindings work in frames and on screens. The innermost of such
-- contexts/objects always gets to handle the key press.

defbindings("WMPlex", {

	bdoc("Increased 5% in volume", "inc_vol"),
	kpress(META.."V", "mod_query.exec_on_merr(_, 'pamixer --increase 5')"),

	bdoc("Decreased 5% in volume", "dec_vol"),
	kpress(META.."Shift+V", "mod_query.exec_on_merr(_, 'pamixer --decrease 5')"),

	bdoc("Restart Notion", "restart"),
	kpress(ALTMETA.."U", "mod_query.query_restart(_)"),

    bdoc("Close current object", "close"),
    kpress_wait(ALTMETA.."C", "WRegion.rqclose_propagate(_, _sub)"),

	-- By using _chld instead of _sub, we can detach/reattach queries
    -- attached to a group. The detach code checks if the parameter
    -- (_chld) is a group 'bottom' and detaches the whole group in that
    -- case.
    bdoc("Detach (float) or reattach an object to its previous location.", "detach"),
    kpress(META.."f", "ioncore.detach(_chld, 'toggle')", "_chld:non-nil"),
})

-- Frames for transient windows ignore this bindmap

defbindings("WMPlex.toplevel", {

    bdoc("Run a terminal emulator.", "kitty"),
    kpress(META.."Shift+Return", "mod_query.exec_on_merr(_, 'kitty')"),

    bdoc("Query for command line to execute.", "run"),
    kpress(META.."p", "mod_query.query_exec(_)"),

    bdoc("Query for a client window to navigate to.", "go"),
    kpress(META.."Shift+G", "mod_query.query_gotoclient(_)"),

    bdoc("Query for a Workspace Group to navigate to.", "ws_go"),
    kpress(ALTMETA.."Shift+G", "mod_query.query_workspace(_)"),

    bdoc("Display context menu.", "ctx"),
    kpress(META.."m", "mod_menu.menu(_, _sub, 'ctxmenu')"),
    bdoc("Query for context menu.", "qctx"),
    kpress(ALTMETA.."m", "mod_query.query_menu(_, _sub, 'ctxmenu', 'Context menu:')"),

    bdoc("Show Notion 'live docs'.", "help"),
    kpress(META.."slash", "notioncore.show_live_docs(_)"),

    bdoc("Query for manual page to be displayed.", "man"),
    kpress(ALTMETA.."slash", "mod_query.query_man(_, ':man')"),
}) 

-- WFrame context bindings
--
-- These bindings are common to all types of frames. Some additional
-- frame bindings are found in some modules' configuration files.

defbindings("WFrame", {
    bdoc("Maximize the frame horizontally.", "hmax"),
    kpress(ALTMETA.."H", "WFrame.maximize_horiz(_)"),
    bdoc("Maximize the frame vertically.", "vmax"),
    kpress(ALTMETA.."V", "WFrame.maximize_vert(_)"),

    bdoc("Begin move/resize mode.", "resize"),
    kpress(META.."R", "WFrame.begin_kbresize(_)"),

    bdoc("Switch the frame to display the object indicated by the tab."),
    mclick("Button1@tab", "WFrame.p_switch_tab(_)"),
    mclick("Button2@tab", "WFrame.p_switch_tab(_)"),

    bdoc("Resize the frame."),
    mdrag("Button1@border", "WFrame.p_resize(_)"),
    mdrag(META.."Button3", "WFrame.p_resize(_)"),

    bdoc("Move the frame."),
    mdrag(META.."Button1", "WFrame.p_move(_)"),

    bdoc("Move objects between frames by dragging and dropping the tab."),
    mdrag("Button1@tab", "WFrame.p_tabdrag(_)"),
    mdrag("Button2@tab", "WFrame.p_tabdrag(_)"),

    bdoc("Switch to next object within the frame.", "->tab"),
    -- See docs on how to disable capslock caps behaviour
    kpress(META.."k", "WFrame.switch_next(_)"),

    bdoc("Switch to previous object within the frame.", "<-tab"),
    kpress(META.."j", "WFrame.switch_prev(_)"),
})

-- Frames for transient windows ignore this bindmap

defbindings("WFrame.toplevel", {
    bdoc("Switch to tab 0 in this frame.", "tab 0"),
    kpress(META.."a", "WFrame.switch_nth(_, 0)"),
    bdoc("Switch to tab 1 in this frame.", "tab 1"),
    kpress(META.."s", "WFrame.switch_nth(_, 1)"),
    bdoc("Switch to tab 2 in this frame.", "tab 2"),
    kpress(META.."d", "WFrame.switch_nth(_, 2)"),
    bdoc("Switch to tab 3 in this frame.", "tab 3"),
    kpress(META.."f", "WFrame.switch_nth(_, 3)"),

    bdoc("Move current tab to the right within the frame.", "tab->"),
    kpress(META.."comma", "WFrame.dec_index(_, _sub)", "_sub:non-nil"),
    bdoc("Move current tab to the left within the frame.", "tab<-"),
    kpress(META.."period", "WFrame.inc_index(_, _sub)", "_sub:non-nil"),

    bdoc("Maximize the frame horizontally/vertically."),
    kpress(ALTMETA.."H", "WFrame.maximize_horiz(_)"),
    kpress(ALTMETA.."V", "WFrame.maximize_vert(_)"),
})

-- Bindings for floating frames

defbindings("WFrame.floating", {
    bdoc("Toggle shade mode"),
    mdblclick("Button1@tab", "WFrame.set_shaded(_, 'toggle')"),

    bdoc("Raise the frame."),
    mpress("Button1@tab", "WRegion.rqorder(_, 'front')"),
    mpress("Button1@border", "WRegion.rqorder(_, 'front')"),
    mclick(META.."Button1", "WRegion.rqorder(_, 'front')"),

    bdoc("Lower the frame."),
    mclick(META.."Button3", "WRegion.rqorder(_, 'back')"),

    bdoc("Move the frame."),
    mdrag("Button1@tab", "WFrame.p_move(_)"),
})


-- WMoveresMode context bindings
--
-- These bindings are available keyboard move/resize mode. The mode
-- is activated on frames with the command begin_kbresize (bound to
-- META.."R" above by default).

defbindings("WMoveresMode", {
    bdoc("Cancel the resize mode."),
    kpress(ALTMETA.."Escape","WMoveresMode.cancel(_)"),

    bdoc("End the resize mode."),
    kpress(ALTMETA.."Return","WMoveresMode.finish(_)"),

    bdoc("Grow in specified direction."),
    kpress("Left",  "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("Right", "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("Up",    "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("Down",  "WMoveresMode.resize(_, 0, 0, 0, 1)"),
    kpress("F",     "WMoveresMode.resize(_, 1, 0, 0, 0)"),
    kpress("B",     "WMoveresMode.resize(_, 0, 1, 0, 0)"),
    kpress("P",     "WMoveresMode.resize(_, 0, 0, 1, 0)"),
    kpress("N",     "WMoveresMode.resize(_, 0, 0, 0, 1)"),

    bdoc("Shrink in specified direction."),
    kpress("Shift+Left",  "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+Right", "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+Up",    "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+Down",  "WMoveresMode.resize(_, 0, 0, 0,-1)"),
    kpress("Shift+F",     "WMoveresMode.resize(_,-1, 0, 0, 0)"),
    kpress("Shift+B",     "WMoveresMode.resize(_, 0,-1, 0, 0)"),
    kpress("Shift+P",     "WMoveresMode.resize(_, 0, 0,-1, 0)"),
    kpress("Shift+N",     "WMoveresMode.resize(_, 0, 0, 0,-1)"),

    bdoc("Move in specified direction."),
    kpress(META.."Left",  "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."Right", "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."Up",    "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."Down",  "WMoveresMode.move(_, 0, 1)"),
    kpress(META.."F",     "WMoveresMode.move(_,-1, 0)"),
    kpress(META.."B",     "WMoveresMode.move(_, 1, 0)"),
    kpress(META.."P",     "WMoveresMode.move(_, 0,-1)"),
    kpress(META.."N",     "WMoveresMode.move(_, 0, 1)"),
})


-- Main menu
defmenu("mainmenu", {
    menuentry("Run...",         "mod_query.query_exec(_)"),
    menuentry("Terminal",       "mod_query.exec_on_merr(_, 'kitty')"),
    menuentry("Help",           "mod_query.query_man(_)"),
    menuentry("About Notion",      "mod_query.show_about_ion(_)"),
    submenu("Styles",           "stylemenu"),
    submenu("Session",          "sessionmenu"),
})


-- Session control menu
defmenu("sessionmenu", {
    menuentry("Save",           "ioncore.snapshot()"),
    menuentry("Restart",        "ioncore.restart()"),
    menuentry("Restart TWM",    "ioncore.restart_other('twm')"),
    menuentry("Exit",           "ioncore.shutdown()"),
})

-- Context menu (frame actions etc.)
defctxmenu("WFrame", "Frame", {
    -- Note: this propagates the close to any subwindows; it does not
    -- destroy the frame itself, unless empty. An entry to destroy tiled
    -- frames is configured in cfg_tiling.lua.
    menuentry("Close",          "WRegion.rqclose_propagate(_, _sub)"),
    -- Low-priority entries
    menuentry("Attach tagged", "ioncore.tagged_attach(_)", { priority = 0 }),
    menuentry("Clear tags",    "ioncore.clear_tags()", { priority = 0 }),
    menuentry("Window info",   "mod_query.show_tree(_, _sub)", { priority = 0 }),
})


-- Context menu for groups (workspaces, client windows)
defctxmenu("WGroup", "Group", {
    menuentry("Toggle tag",     "WRegion.set_tagged(_, 'toggle')"),
    menuentry("De/reattach",    "ioncore.detach(_, 'toggle')"),
})


-- Context menu for workspaces
defctxmenu("WGroupWS", "Workspace", {
    menuentry("Close",          "WRegion.rqclose(_)"),
    menuentry("Rename",         "mod_query.query_renameworkspace(nil, _)"),
    menuentry("Attach tagged",  "ioncore.tagged_attach(_)"),
})


-- Context menu for client windows
defctxmenu("WClientWin", "Client window", {
    menuentry("Kill",           "WClientWin.kill(_)"),
})
