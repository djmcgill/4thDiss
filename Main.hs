import Netwire.SFML (adaptSimple)

import Engine.Display (drawWorld)
import Engine.World (worldWire)

main = adaptSimple "testSFMLAndRigidBody" drawWorld worldWire