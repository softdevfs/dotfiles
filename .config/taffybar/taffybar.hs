{-# LANGUAGE OverloadedStrings #-}

import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Icon
import System.Taffybar.Widget.Generic.PollingLabel

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

myClockConfig :: ClockConfig
myClockConfig = defaultClockConfig
  { clockFormatString = "  %d-%m-%Y | %H:%M  " }

myWorkspaceConfig :: WorkspacesConfig
myWorkspaceConfig = defaultWorkspacesConfig
  { maxIcons = Just 0 }

main :: IO ()
main = do

  let clock = textClockNewWith myClockConfig
      clockIcon = iconImageWidgetNew ".config/taffybar/icons/clock.png"
      cpu = textCpuMonitorNew "  $total$%  " 1
      cpuIcon = iconImageWidgetNew ".config/taffybar/icons/cpu.png"
      mem = textMemoryMonitorNew "  $used$  " 1
      memIcon = iconImageWidgetNew ".config/taffybar/icons/memory.png"
      workspaces = workspacesNew myWorkspaceConfig
      simpleConfig = defaultSimpleTaffyConfig
                       { startWidgets = [ workspaces ]
                       , endWidgets = [ clock, clockIcon, cpu, cpuIcon, mem, memIcon]
                       , barPosition = Top
		       , barHeight = 35
                       , barPadding = 0
                       , widgetSpacing = 0
                       }
  simpleTaffybar simpleConfig
