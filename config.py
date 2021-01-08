
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

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.lazy import lazy
from libqtile import layout, bar, widget
from typing import List  # noqa: F401

mod = "mod4"

keys = [
    # Custom layout keys
    # XMonadTall
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "control"], "k", lazy.layout.grow()),
    Key([mod, "control"], "j", lazy.layout.shrink()),
    Key([mod, "shift"], "n", lazy.layout.normalize()),
    # Key([mod], "n", lazy.layout.maximize()),
    Key([mod, "shift"], "space", lazy.layout.flip()),
    Key([mod], "Return", lazy.layout.toggle_split()),
    # Bsp
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "mod1"], "j", lazy.layout.flip_down()),
    Key([mod, "mod1"], "k", lazy.layout.flip_up()),
    Key([mod, "mod1"], "h", lazy.layout.flip_left()),
    Key([mod, "mod1"], "l", lazy.layout.flip_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    # Switch between windows in current stack pane
    # Key([mod], "k", lazy.layout.down()),
    # Key([mod], "j", lazy.layout.up()),

    # Move windows up or down in current stack
    # Key(["mod1", "shift"], "k", lazy.layout.shuffle_down()),
    # Key(["mod1", "shift"], "j", lazy.layout.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key(["mod1"], "Tab", lazy.layout.next()),

    
    # Swap panes of split stack
    # Key([mod, "shift"], "space", lazy.layout.rotate()),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    # Key([mod, "shift"], "s", lazy.layout.toggle_split()),
    Key(["control", "mod1"], "t", lazy.spawn("tilix")),

    # Run rofi
    Key(["mod1"], "w", lazy.spawn("rofi -show window -theme gruvbox-dark")),
    Key(["mod1"], "r", lazy.spawn("rofi -show run -theme gruvbox-dark")),

    # Toggle between different layouts as defined below
    Key([mod], "Return", lazy.next_layout()),
    # default closing windows:    Key([mod], "w", lazy.window.kill()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),

    # Close Window
    Key(["mod1"], "F4", lazy.window.kill()),

    # Change Wallpaper
    Key([mod], "r", lazy.spawn("python /home/sergio/Scripts/change_wallpaper.py")),

    # Volume
    Key(["mod1"], "F3", lazy.spawn("pamixer -i 3")),
    Key(["mod1"], "F2", lazy.spawn("pamixer -d 3")),
    Key(["mod1"], "F1", lazy.spawn("pamixer -t")),

    # Play Youtube Videos in High Quality
    Key([mod], "y", lazy.spawn("/home/sergio/Scripts/Bash/youtube_mpv.sh")),

    # Mute and unmute microphone
    Key([mod], "m", lazy.spawn("pamixer --source 2 -m")),
    Key([mod, "shift"], "m", lazy.spawn("pamixer --source 2 -u")),

    # Lock screen
    Key([mod], "x", lazy.spawn("i3lock -c 252525")),

    # Minimize & float window
    Key([mod], "w", lazy.window.toggle_minimize()),
    Key([mod, "shift"], "w", lazy.window.toggle_floating()),

    # Open dmenu
    Key([mod], "b", lazy.spawn("dmenu_run")),

    # Transparency
    # Key([mod, "control"], "t", lazy.spawn("picom-trans -c -7")),
    # Key([mod, "control"], "y", lazy.spawn("picom-trans -c +7")),

    # Open pdf without file manager
    Key([mod, "shift"], "b", lazy.spawn("/home/sergio/Scripts/Bash/open_pdf.sh")),

    # Open jgmenu
    Key(["control", "mod1"], "j", lazy.spawn("jgmenu_run")),
]

groups = [Group(i) for i in "asdfuiop"]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen()),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

layouts = [
    # layout.MonadTall(border_focus='green', border_normal='#4E4A4A', border_width=3, margin=5),
    layout.Bsp(border_focus='green', border_normal='#4E4A4A', border_width=3, margin=5),
    layout.Floating(),
    layout.Max(),
    # layout.Stack(num_stacks=2),
    # Try more layouts by unleashing below layouts.
    # layout.Columns(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]
##### COLORS #####
#colors = [
#          ["#1a1a1a", "#000000"], # font color for group names
#         ]

colors = [["#292d3e", "#292d3e"], # panel background
          ["#434758", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#8d62a9", "#8d62a9"], # border line color for other tab and odd widgets
          ["#668bd7", "#668bd7"], # color for the even widgets
          ["#e1acff", "#e1acff"]] # window name

colors_two = {
    "black":		["#2B303B", "#2B303B"],
    "grey":			["#40444D", "#424A5B"],
    "white":		["#C0C5CE", "#C0C5CE"],
    "red":			["#BF616A", "#BF616A"],
    "magenta":		["#B48EAD", "#B48EAD"],
    "green":		["#A3BE8C", "#A3BE8C"],
    "darkgreen":	["#859900", "#859900"],
    "blue":			["#8FA1B3", "#8FA1B3"],
    "darkblue":		["#65737E", "#65737E"],
    "orange":		["#EBCB8B", "#EBCB8B"]
}

widget_defaults = dict(
    font='DaddyTimeMono Nerd Font Mono',
    fontsize=14,
    padding=3,
    background=colors[0],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayout(),
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
                widget.Prompt(),
                widget.WindowName(),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar06.png",
                    background = colors_two["red"]
                ),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar02-b.png",
                    background = colors_two["red"]
                ),
                widget.TextBox(
                    text="b34n5💻",
                    background = colors_two["red"],
                    foreground = colors_two["black"]
                ),
                widget.Systray(),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar03.png",
                    background = colors_two["magenta"]
                ),
                widget.Clock(
                    foreground = colors_two["black"],
                    background = colors_two["magenta"],
                    format='%d-%m-%Y %a %H:%M %p'
                ),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar04.png",
                    background = colors_two["green"]
                ),
				widget.PulseVolume(
				    foreground = colors_two["black"],
				    background = colors_two["green"],
				),
                # widget.QuickExit(),
                # widget.Sep(),
                widget.Image(
        			scale = True,
        			filename = "/home/sergio/.config/qtile/icons/bar05.png",
        			background = colors_two["blue"]
        		),
                widget.Wlan(
                    background = colors_two["blue"],
                    foreground = colors_two["black"],
                    interface='wlp3s0'
                ),
            ],
            24,
	    #background="#0d0d0d",
        ),
	#wallpaper='/home/sergio/Pictures/wallpapers/anime-girl-01.jpg',
	#wallpaper_mode='fill',
    ),
    Screen(
        top=bar.Bar(
            [
                widget.CurrentLayout(),
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
                widget.Prompt(),
                widget.WindowName(),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar06.png",
                    background = colors_two["red"]
                ),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar02-b.png",
                    background = colors_two["red"]
                ),
                widget.TextBox(
                    text="b34n5💻",
                    background = colors_two["red"],
                    foreground = colors_two["black"]
                ),
                widget.Systray(),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar03.png",
                    background = colors_two["magenta"]
                ),
                widget.Clock(
                    foreground = colors_two["black"],
                    background = colors_two["magenta"],
                    format='%d-%m-%Y %a %H:%M %p'
                ),
                widget.Image(
                    scale = True,
                    filename = "/home/sergio/.config/qtile/icons/bar04.png",
                    background = colors_two["green"]
                ),
				widget.PulseVolume(
				    foreground = colors_two["black"],
				    background = colors_two["green"],
				),
                # widget.QuickExit(),
                # widget.Sep(),
                widget.Image(
        			scale = True,
        			filename = "/home/sergio/.config/qtile/icons/bar05.png",
        			background = colors_two["blue"]
        		),
                widget.Wlan(
                    background = colors_two["blue"],
                    foreground = colors_two["black"],
                    interface='wlp3s0'
                ),
            ],
            24,
	    #background="#0d0d0d",
        ),
	#wallpaper='/home/sergio/Pictures/wallpapers/anime-girl-01.jpg',
	#wallpaper_mode='fill',
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
