{-
  This file is part of BajaMud.
    
  BajaMud -- MUD client library
  Copyright (C) 2011  Endre Tamas SAJO

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Game.BajaMud.Layout
  --(
  --)
  where

import Graphics.Vty
import Graphics.Vty.Widgets.All

data ScrollText = MkScrollText
  { textLines :: [String]
  , textWidget :: Widget FormattedText
  }

