# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen, EzKey
from libqtile.lazy import lazy

mod = "mod1"
terminal = "kitty"
groups = [Group(i) for i in [" www ", " dev ", " sys ", " mus ", " chat ", " doc ", " vid ", " vbox ", " gfx "] ]


keys = [
    EzKey("A-j", lazy.layout.left(), desc="Move focus to left"),
    EzKey("A-k", lazy.layout.right(), desc="Move focus to right"),
    EzKey("A-<Tab>", lazy.layout.next(), desc="Move window focus to other window"),
    EzKey("A-S-j", lazy.layout.shuffle_left(), desc="Move window to the left"),
    EzKey("A-S-k", lazy.layout.shuffle_right(), desc="Move window to the right"),
    EzKey("A-S-h", lazy.layout.shuffle_down(), desc="Move window down"),
    EzKey("A-S-l", lazy.layout.shuffle_up(), desc="Move window up"),
    EzKey("A-h", lazy.layout.grow_left(), desc="Grow window to the left"),
    EzKey("A-l", lazy.layout.grow_right(), desc="Grow window to the right"),
    EzKey("A-t", lazy.layout.normalize(), desc="Reset all window sizes"),
    EzKey("A-<Space>", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
    EzKey("A-S-<Return>", lazy.spawn(terminal), desc="Launch terminal"),
    EzKey("A-<Space>", lazy.next_layout(), desc="Toggle between layouts"),
    EzKey("A-S-c", lazy.window.kill(), desc="Kill focused window"),
    EzKey("C-A-f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen on the focused window"),
    EzKey("C-A-z", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
    EzKey("A-q", lazy.reload_config(), desc="Reload the config"),
    EzKey("A-S-q", lazy.shutdown(), desc="Shutdown Qtile"),
    # MonadTall specific ##############
    EzKey("A-k", lazy.layout.grow(), desc="Grow window"),
    EzKey("A-j", lazy.layout.shrink(), desc="Shrink window"),
    EzKey("A-S-j", lazy.layout.swap_left(), desc="Move window to the left"),
    EzKey("A-S-k", lazy.layout.swap_right(), desc="Move window to the right"),
    ###################################
    # Programs
    EzKey("A-v", lazy.spawn("pamixer -i 5"), desc="Increase volume"),
    EzKey("A-S-v", lazy.spawn("pamixer -d 5"), desc="Decrease volume"),
    EzKey("A-f", lazy.spawn("firefox-esr"), desc="Launch firefox"),
    EzKey("A-c", lazy.spawn("emacs"), desc="Launch emacs"),
    EzKey("A-i", lazy.spawn("kitty irssi"), desc="Launch irssi"),
    EzKey("A-s", lazy.spawn("scrot ~/pictures/screenshots/screenshot.png"), desc="Take screenshot"),
    EzKey("A-C-s", lazy.spawn("scrot -s ~/pictures/screenshots/screenshot.png"), desc="Take a selected screenshot"),
    EzKey("A-S-s", lazy.spawn("kitty screenRecorder"), desc="Capture desktop"),
    ###################################
    # Workspaces
    EzKey("A-p", lazy.spawn("dmenu_run -fn 'Mononoki-16'"), desc="Spawn a command using a prompt widget"),
    EzKey("A-1", lazy.group[" www "].toscreen(), desc="Switch to www workspace"),
    EzKey("A-2", lazy.group[" dev "].toscreen(), desc="Switch to dev workspace"),
    EzKey("A-3", lazy.group[" sys "].toscreen(), desc="Switch to sys workspace"),
    EzKey("A-4", lazy.group[" mus "].toscreen(), desc="Switch to mus workspace"),
    EzKey("A-5", lazy.group[" chat "].toscreen(), desc="Switch to chat workspace"),
    EzKey("A-6", lazy.group[" doc "].toscreen(), desc="Switch to doc workspace"),
    EzKey("A-7", lazy.group[" vid "].toscreen(), desc="Switch to vid workspace"),
    EzKey("A-8", lazy.group[" vbox "].toscreen(), desc="Switch to vbox workspace"),
    EzKey("A-9", lazy.group[" gfx "].toscreen(), desc="Switch to gfx workspace"),
    EzKey("A-S-1", lazy.window.togroup(" www ", switch_group=False), desc="Move focused window to www workspace"),
    EzKey("A-S-2", lazy.window.togroup(" dev ", switch_group=False), desc="Move focused window to dev workspace"),
    EzKey("A-S-3", lazy.window.togroup(" sys ", switch_group=False), desc="Move focused window to sys workspace"),
    EzKey("A-S-4", lazy.window.togroup(" mus ", switch_group=False), desc="Move focused window to mus workspace"),
    EzKey("A-S-5", lazy.window.togroup(" chat ", switch_group=False), desc="Move focused window to chat workspace"),
    EzKey("A-S-6", lazy.window.togroup(" doc ", switch_group=False), desc="Move focused window to doc workspace"),
    EzKey("A-S-7", lazy.window.togroup(" vid ", switch_group=False), desc="Move focused window to vid workspace"),
    EzKey("A-S-8", lazy.window.togroup(" vbox ", switch_group=False), desc="Move focused window to vbox workspace"),
    EzKey("A-S-9", lazy.window.togroup(" gfx ", switch_group=False), desc="Move focused window to gfx workspace"),
    ###################################
]

layouts = [
    layout.MonadTall(align=0, border_focus="#00e673", border_normal="#ff0000", margin=5),
    layout.MonadTall(align=1, border_focus="#00e673", border_normal="#ff0000", margin=5),
    layout.Max(border_focus="#00e673", border_normal="#ff0000", margin=5)
    # layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    # layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="Mononoki",
    fontsize=16,
    padding=3,
)
extension_defaults = widget_defaults.copy()

colors = [["#292d3e", "#292d3e"], # panel background
          ["#434758", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#8d62a9", "#8d62a9"], # border line color for other tab and odd widgets
          ["#668bd7", "#668bd7"], # color for the even widgets
          ["#e1acff", "#e1acff"]] # window name

colors_two = {
    "black":		["#2B303B", "#2B303B"],
    "grey":		["#40444D", "#424A5B"],
    "white":		["#C0C5CE", "#C0C5CE"],
    "red":		["#BF616A", "#BF616A"],
    "magenta":		["#B48EAD", "#B48EAD"],
    "green":		["#A3BE8C", "#A3BE8C"],
    "darkgreen":	["#859900", "#859900"],
    "blue":		["#8FA1B3", "#8FA1B3"],
    "darkblue":		["#65737E", "#65737E"],
    "orange":		["#EBCB8B", "#EBCB8B"]
}


screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    active = colors[2],
                    inactive = colors[2],
                    rounded = False,
                    highlight_color = colors[1],
                    highlight_method = "line",
                    this_current_screen_border = colors[3],
                    this_screen_border = colors [4],
                    other_current_screen_border = colors[0],
                    other_screen_border = colors[0],
                    foreground = colors[2],
                    background = colors[0]
                ),
                widget.Sep(background=colors[0], foreground=colors[2], padding=10),
                widget.CurrentLayout(foreground=colors[2], background=colors[0]),
                widget.Sep(background=colors[0], foreground=colors[2], padding=10),
                widget.WindowName(foreground=colors[2], background=colors[0]),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/icons/bar06.png",
                    background = colors_two["red"]
                ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/icons/bar02-b.png",
                    background = colors_two["red"]
                ),
                widget.TextBox(
                    text="b34n5💻",
                    background = colors_two["red"],
                    foreground = colors_two["black"]
                ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/icons/bar03.png",
                    background = colors_two["magenta"]
                ),
                widget.Clock(
                    foreground = colors_two["black"],
                    background = colors_two["magenta"],
                    format='%d-%m-%Y %a %H:%M %p'
                ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/icons/bar04.png",
                    background = colors_two["green"]
                ),
		widget.PulseVolume(
		    foreground = colors_two["black"],
		    background = colors_two["green"],
                    limit_max_volume = True,
                    volume_app = "pamixer"
		),
                widget.Image(
        	    scale = True,
        	    filename = "~/.config/qtile/icons/bar05.png",
        	    background = colors_two["blue"]
        	),
                widget.Memory(
                    background = colors_two["blue"],
                    foreground = colors_two["black"]
                ),
            ],
            24,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
