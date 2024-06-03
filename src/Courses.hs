module Courses where

data Status = Passed | Planned Int

type ISPCourse = (Course, Status)
type CourseCode = String

--data Status = Finished | Planned Int

data Period = FirstSem | SecondSem | AllYear deriving (Show, Eq, Ord)
data Course = Course
  { name :: String
  , code :: CourseCode
  , description :: String
  , period :: Period
  , studyPoints :: Int
  } deriving (Show, Eq)

bedrijfskunde :: Course
bedrijfskunde = Course
  { name = "Bedrijfskunde en Entrepreneurship"
  , code = "H01F2A"
  , description = ""
  , period = FirstSem
  , studyPoints = 6
  }

H04I0A :: Course
H04I0A = Course
  { name = "Industrial Internet Infrastructure"
  , code = "H04I0A"
  , description = ""
  , period = SecondSem
  , studyPoints = 5
  }

G0K31B :: Course
G0K31B = Course
    { name = "Gedistribueerde softwarearchitecturen: verdiepende studie"
    , code = "G0K31B"
    , description = ""
    , period = SecondSem
    , studyPoints = 4
    }

H04G7A :: Course
H04G7A = Course
  { name = "Capita Selecta Computer Science: Distributed Systems"
  , code = "H04G7A"
  , description = ""
  , period = AllYear
  , studyPoints = 4
  }

H04L2A :: Course
H04L2A = Course
  { name = "Software for Real-time and Embedded Systems"
  , code = "H04L2A"
  , description = ""
  , period = FirstSem
  , studyPoints = 4
  }

-- Uitzonderlijk niet ingericht dit jaar. Hoe implementeer ik dit?
--H02H4A :: Course
--H02H4A = Course
--  { name = "Multi-Agent Systems"
--  , code = "H02H4A"
--  , description = ""
--  , period = FirstSem
--  , studyPoints = 4
--  }

H0A12A :: Course
H0A12A = Course
  { name = "Geavanceerde methodes voor softwarearchitectuur"
  , code = "H0A12A"
  , description = ""
  , period = FirstSem
  , studyPoints = 4
  }

-- Secure software master

--- Obligatory

H04K5A :: Course
H04K5A = Course
  { name = "Development of Secure Software"
  , code = "H04K5A"
  , description = ""
  , period = FirstSem
  , studyPoints = 4
  }

H04G4A :: Course
H04G4A = Course
  { name = "Security Governance and Operations"
  , code = "H04G4A"
  , description = ""
  , period = SecondSem
  , studyPoints = 4
  }

H05M8B :: Course
H05M8B = Course
  { name = "Capita Selecta Computer Science: Secure Software"
  , code = "H05M8B"
  , description = ""
  , period = SecondSem
  , studyPoints = 6
  }

--- Optional

H04L2A :: Course
H04L2A = Course
  { name = "Software for Real-time and Embedded Systems"
  , code = "H04L2A"
  , description = ""
  , period = FirstSem
  , studyPoints = 4
  }

G0K32A :: Course
G0K32A = Course
  { name = "Vereistenanalyse voor complexe softwaresystemen"
  , code = "G0K32A"
  , description = ""
  , period = FirstSem
  , studyPoints = 3
  }

H04H8B :: Course
H04H8B = Course
  { name = "Formal Systems and their Applications"
  , code = "H04H8B"
  , description = ""
  , period = FirstSem
  , studyPoints = 6
  }

H0Q31A :: Course
H0Q31A = Course
  { name = "Security Through the Software Lifecycle"
  , code = "H0Q31A"
  , description = ""
  , period = FirstSem
  , studyPoints = 4
  }

H05D9A :: Course
H05D9A = Course
  { name = "Cryptografie en netwerkbeveiliging"
  , code = "H05D9A"
  , description = ""
  , period = SecondSem
  , studyPoints = 3
  }

-- Artificial Intelligence

