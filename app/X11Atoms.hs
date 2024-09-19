module X11Atoms where

import Graphics.X11 (Atom, Display, internAtom)

data Atoms
  = Atoms
  { atoms_TARGETS :: Atom
  }

initAtoms :: Display -> IO Atoms
initAtoms display = do
  tARGETS <- internAtom display "TARGETS" False
  pure
    Atoms
    { atoms_TARGETS = tARGETS
    }
