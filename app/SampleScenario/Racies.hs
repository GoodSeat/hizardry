module SampleScenario.Racies where

import qualified Data.Items as Item

import Data.Primitive
import Data.Formula
import qualified Data.Characters as Character


racies = [dwarf, gnome, hobbit, human, elf]

human = Character.Race {
    Character.raceName = "Human"
  , Character.initialParam = Parameter {
      strength = 8, iq = 8, piety = 8, vitality = 8, agility = 8, luck = 8
  }
  , Character.maxParam = Parameter {
      strength = 18, iq = 18, piety = 18, vitality = 18, agility = 18, luck = 18
  }
  , Character.initialBonus = bonus
}

elf = Character.Race {
    Character.raceName = "Elf"
  , Character.initialParam = Parameter {
      strength = 7, iq = 10, piety = 10, vitality = 7, agility = 9, luck = 7
  }
  , Character.maxParam = Parameter {
      strength = 17, iq = 20, piety = 20, vitality = 17, agility = 19, luck = 17
  }
  , Character.initialBonus = bonus
}

dwarf = Character.Race {
    Character.raceName = "Dwarf"
  , Character.initialParam = Parameter {
      strength = 10, iq = 7, piety = 10, vitality = 10, agility = 6, luck = 7
  }
  , Character.maxParam = Parameter {
      strength = 20, iq = 17, piety = 20, vitality = 20, agility = 16, luck = 17
  }
  , Character.initialBonus = bonus
}

gnome = Character.Race {
    Character.raceName = "Gnome"
  , Character.initialParam = Parameter {
      strength = 8, iq = 8, piety = 11, vitality = 9, agility = 8, luck = 6
  }
  , Character.maxParam = Parameter {
      strength = 18, iq = 18, piety = 21, vitality = 19, agility = 18, luck = 16
  }
  , Character.initialBonus = bonus
}

hobbit = Character.Race {
    Character.raceName = "Hobbit"
  , Character.initialParam = Parameter {
      strength = 6, iq = 7, piety = 7, vitality = 8, agility = 10, luck = 12
  }
  , Character.maxParam = Parameter {
      strength = 16, iq = 17, piety = 17, vitality = 18, agility = 20, luck = 22
  }
  , Character.initialBonus = bonus
}

bonus = parse' "min(60, 4+1d5+max(0,1d10-9)*10+max(0,1d100-99)*20+max(0,1d1000-999)*30)"
