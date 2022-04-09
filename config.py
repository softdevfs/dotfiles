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

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"

keys = [
    # Custom layout keys
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    Key([mod], "Left", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod], "Right", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod], "Down", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod], "Up", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod, "shift"], "Left", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "shift"], "Right", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "shift"], "Down", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "shift"], "Up", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod, "shift"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod, "shift"], "q", lazy.window.kill(), desc="Kill focused window"),
    Key(["control", "mod1"], "t", lazy.spawn("kitty")),
    Key(["mod1"], "w", lazy.spawn("rofi -show window -theme gruvbox-dark")),
    Key(["mod1"], "r", lazy.spawn("rofi -show run -theme gruvbox-dark")),
    Key(["mod1"], "e", lazy.spawn("rofi -show emoji -modi emoji -theme gruvbox-dark")),
    Key([mod], "Return", lazy.next_layout()),
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod, "shift"], "r", lazy.spawncmd()),
    Key([mod], "r", lazy.spawn("python3 /home/sergio/Scripts/change_wallpaper.py")),

    # Volume
    Key(["mod1"], "F3", lazy.spawn("pamixer -i 3")),
    Key(["mod1"], "F2", lazy.spawn("pamixer -d 3")),
    Key(["mod1"], "F1", lazy.spawn("pamixer -t")),

    # Mute and unmute microphone
    Key([mod], "m", lazy.spawn("pamixer --source 2 -m")),
    Key([mod, "shift"], "m", lazy.spawn("pamixer --source 2 -u")),

    # Minimize & float window
    Key([mod], "w", lazy.window.toggle_minimize()),
    Key([mod, "shift"], "w", lazy.window.toggle_floating()),
]

groups = [Group(i) for i in "asdfuiop"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Bsp(border_focus='green', border_normal='#4E4A4A', border_width=0, margin=5),
    layout.Floating(),
    layout.Max(),
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
    font="DaddyTimeMono Nerd Font Mono",
    fontsize=14,
    padding=5,
    background=colors[0]
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
                widget.Memory(
                    background = colors_two["blue"],
                    foreground = colors_two["black"],
                ),
            ],
            24,
        ),
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
                widget.Memory(
                    background = colors_two["blue"],
                    foreground = colors_two["black"]                ),
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
