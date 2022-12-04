module Base where
import Data.List

type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
  deriving(Eq, Show)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
  deriving(Eq, Show)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
  deriving(Eq, Show)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 0

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2



tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun plants zombies) (x, y) plant
  | (sun < (getPlantCost plant)) || (x >= 5) || (y >= 12) || (x < 0) || (y < 0) = Nothing -- Nothing cases where the plant is either too expensive or would be placed outside of the map
  | (lookup (x, y) plants) == Nothing = Just (GameModel (sun-(getPlantCost plant)) (((x,y), plant):plants) zombies)
  | otherwise = Nothing

getPlantCost :: Plant -> Int
getPlantCost (Peashooter _) = 100
getPlantCost (Sunflower _) = 50
getPlantCost (Walnut _) = 50
getPlantCost (CherryBomb _) = 150



placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) zombie lane
  | (lane >= 5) || (lane < 0) = Nothing
  | (lookup (lane, 11) zombies) == Nothing = Just (GameModel sun plants (((lane, 11), zombie):zombies))
  | otherwise = Nothing



performZombieActions :: GameModel -> Maybe GameModel
performZombieActions gm@(GameModel s [] zombies)
  | isGameOver gm = Nothing
  | otherwise = Just (GameModel s [] (map (\x -> moveZombie x) zombies))
performZombieActions (GameModel s plants zombies) = Just (GameModel s (damagePlants plants zombies) (performZombies (map (\currentZombie -> map (\currentPlant -> isZombieAbleToMove currentZombie currentPlant) plants) zombies) zombies))


performZombies :: [[Int]] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
performZombies [] _ = []
performZombies (cIntList:rIntLists) (cZombie:rZombies) = (performZombie cIntList cZombie) : performZombies rIntLists rZombies

performZombie :: [Int] -> (Coordinate, Zombie) -> (Coordinate, Zombie)
performZombie l zombie
  | (filter (\x -> x == 0) l ) /= [] = zombie
  | (filter (\x -> x == 1) l ) /= [] = moveVaulting zombie 1
  | (filter (\x -> x == 2) l ) /= [] = moveVaulting zombie 2
  | otherwise = moveZombie zombie


damagePlants :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]
damagePlants plants zombies = map (\plant -> damagePlant plant zombies) plants

damagePlant :: (Coordinate, Plant) -> [(Coordinate, Zombie)] -> (Coordinate, Plant)
damagePlant plant zombies = reducePlantHp plant (length (filter (\each -> each == True) (map (\currentZombie -> (shouldZombieAttack currentZombie plant)) zombies)))


isGameOver :: GameModel -> Bool
isGameOver (GameModel _ _ []) = False
isGameOver (GameModel s p (((_, y), _):rest))
  | y <= 0 = True
  | otherwise = isGameOver (GameModel s p rest)


isVaultingZombie :: (Coordinate, Zombie) -> Bool
isVaultingZombie (_, (Vaulting _ _ )) = True
isVaultingZombie _ = False

vaultingCanJump :: (Coordinate, Zombie) -> Bool
vaultingCanJump (_, (Vaulting _ speed))
  | speed > 1 = True
  | otherwise = False
vaultingCanJump _ = False


shouldZombieAttack :: (Coordinate, Zombie) -> (Coordinate, Plant) -> Bool
shouldZombieAttack zombie@(zombieCoords, _) (plantCoords, _)
  | (zombieCoords == plantCoords) && (isVaultingZombie zombie) && (vaultingCanJump zombie) = False
  | zombieCoords == plantCoords = True
  | otherwise = False

isZombieAbleToMove :: (Coordinate, Zombie) -> (Coordinate, Plant) -> Int
isZombieAbleToMove zombie@(zombieCoords, _) plant@(plantCoords, _)
  | (zombieCoords == plantCoords) && (isVaultingZombie zombie) = 1 -- is standing but can jump
  | zombieCoords == plantCoords = 0 -- cant move -> attack
  | isPlantInPath zombie plant = 2 -- can move but plant is in path
  | otherwise = 3 -- canMove

isPlantInPath :: (Coordinate, Zombie) -> (Coordinate, Plant) -> Bool
isPlantInPath ((zX, zY), (Vaulting _ speed)) ((pX, pY), _) = ((zY-1) == pY) && (zX == pX)
isPlantInPath _ _ = False

moveZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
moveZombie ((x,y), z@(Basic _ speed)) = ((x, (y-speed)), z)
moveZombie ((x,y), z@(Conehead _ speed)) = ((x, (y-speed)), z)
moveZombie ((x,y), z@(Buckethead _ speed)) = ((x, (y-speed)), z)
moveZombie ((x,y), z@(Vaulting _ speed)) = ((x, (y-speed)), z)

moveVaulting :: (Coordinate, Zombie) -> Int -> (Coordinate, Zombie)
moveVaulting zombie@((x,y), (Vaulting hp speed)) jump
  | jump == 2 = ((x, (y-speed)), (Vaulting hp (speed-1)))
  | jump == 1 = ((x, (y-1)), (Vaulting hp (speed-1)))
  | otherwise = ((x, (y-speed)), (Vaulting hp speed))


reducePlantHp :: (Coordinate, Plant) -> Int -> (Coordinate, Plant)
reducePlantHp (c,(Peashooter hp)) amount = (c, (Peashooter (hp-amount)))
reducePlantHp (c, (Sunflower hp)) amount = (c, (Sunflower (hp-amount)))
reducePlantHp (c, (Walnut hp) ) amount = (c, (Walnut (hp-amount)))
reducePlantHp (c, (CherryBomb hp)) amount = (c, (CherryBomb (hp-amount)))



isZombieDead :: Zombie -> Bool
isZombieDead (Basic hp _) = hp <= 0
isZombieDead (Conehead hp _) = hp <= 0
isZombieDead (Buckethead hp _) = hp <= 0
isZombieDead (Vaulting hp _) = hp <= 0

isPlantDead :: Plant -> Bool
isPlantDead (Peashooter hp) = hp <= 0
isPlantDead (Sunflower hp) = hp <= 0
isPlantDead (Walnut hp) = hp <= 0
isPlantDead (CherryBomb hp) = hp <= 0

removeDeadZombies :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
removeDeadZombies [] = []
removeDeadZombies ((coords, zombie):rest)
  | isZombieDead zombie = removeDeadZombies rest
  | otherwise = (coords, zombie) : removeDeadZombies rest

removeDeadPlants :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
removeDeadPlants [] = []
removeDeadPlants ((coords, plant):rest)
  | isPlantDead plant = removeDeadPlants rest
  | otherwise = (coords, plant) : removeDeadPlants rest


cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel s plants zombies) = (GameModel s (removeDeadPlants plants) (removeDeadZombies zombies))



{- 
  performPlantActions (GameModel 0 (replicate 5 ((0,0), defaultSunflower)) []) == GameModel 125 (replicate 5 ((0,0), defaultSunflower)) []
  performPlantActions (GameModel 0 [((0,0), defaultPeashooter)] [((0,3), coneHead)]) == GameModel 0 [((0,0), defaultPeashooter)] [((0,3), Conehead 9 1)]
  performPlantActions (GameModel 0 [((3,3), defaultCherryBomb )] [((2,2), bucketHead)]) == GameModel 0 [((3,3),CherryBomb (-1))] [((2,2),Buckethead (-1) 1)]
-}
performPlantActions :: GameModel -> GameModel
performPlantActions (GameModel s plants []) = (GameModel (s+(getSunflowers plants)) plants [])
performPlantActions (GameModel s plants zombies) = (GameModel (s+getSunflowers plants) (performPlants plants) (damageZombies plants zombies))


performPlants :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
performPlants [] = []
performPlants (cPlant:rPlants)
  | isCherrybomb cPlant = (killCherrybomb cPlant) : performPlants rPlants
  | otherwise = cPlant : performPlants rPlants

damageZombies :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
damageZombies _ [] = []
damageZombies plants (cZombie:rZombies)
  | filter (==2) (map (\plant -> damageZombie plant cZombie) plants) /= [] = (killZombie cZombie) : damageZombies plants rZombies
  | filter (==1) (map (\plant -> damageZombie plant cZombie) plants) /= [] = (reduceZombieHp cZombie (length (filter (==1) (map (\plant -> damageZombie plant cZombie) plants)))) : damageZombies plants rZombies
  | otherwise = cZombie : damageZombies plants rZombies

damageZombie :: (Coordinate, Plant) -> (Coordinate, Zombie) -> Int
damageZombie plant@(plantCoords@(plantX, plantY), _) zombie@(zombieCoords@(zombieX, zombieY), _)
  | (isPeashooter plant) && (plantX == zombieX) && (zombieY >= plantY) = 1
  | (isCherrybomb plant) && isInCherrybombRange plantCoords zombieCoords = 2
  | otherwise = 0
  

isInCherrybombRange :: Coordinate -> Coordinate -> Bool
isInCherrybombRange plantCoords@(plantX, plantY) zombieCoords@(zombieX, zombieY)
  | plantCoords == zombieCoords = True
  | (plantX-1 == zombieX && plantY-1 == zombieY) || (plantX-1 == zombieX && plantY == zombieY) || (plantX-1 == zombieX && plantY+1 == zombieY) || (plantX == zombieX && plantY-1 == zombieY) || (plantX == zombieX && plantY+1 == zombieY) || (plantX+1 == zombieX && plantY-1 == zombieY) || (plantX+1 == zombieX && plantY == zombieY) || (plantX+1 == zombieX && plantY+1 == zombieY) = True
  | otherwise = False


-- visual rep. of cheerybomb radius
-- (x-1, y-1) (x-1, y) (x-1, y+1)
-- (x, y-1) c (x, y+1)
-- (x+1, y-1) (x+1, y) (x+1, y+1)

isPeashooter :: (Coordinate, Plant) -> Bool
isPeashooter (_, (Peashooter _)) = True
isPeashooter _ = False

isCherrybomb :: (Coordinate, Plant) -> Bool
isCherrybomb (_, (CherryBomb _)) = True
isCherrybomb _ = False

getSunflowers :: [(Coordinate, Plant)] -> Int
getSunflowers [] = 0
getSunflowers ((_, Sunflower _):rest) = 25 + getSunflowers rest
getSunflowers (_:rest) = getSunflowers rest

killCherrybomb :: (Coordinate, Plant) -> (Coordinate, Plant)
killCherrybomb (coords, (CherryBomb hp)) = (coords, (CherryBomb (-1)))


killZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
killZombie (coords,(Basic hp s)) = (coords, (Basic 0 s))
killZombie (coords,(Conehead hp s)) = (coords, (Conehead 0 s))
killZombie (coords,(Buckethead hp s)) = (coords, (Buckethead 0 s))
killZombie (coords,(Vaulting hp s)) = (coords, (Vaulting 0 s))

reduceZombieHp :: (Coordinate, Zombie) -> Int -> (Coordinate, Zombie)
reduceZombieHp (coords,(Basic hp s)) amount = (coords, (Basic (hp-amount) s))
reduceZombieHp (coords,(Conehead hp s)) amount = (coords, (Conehead (hp-amount) s))
reduceZombieHp (coords,(Buckethead hp s)) amount = (coords, (Buckethead (hp-amount) s))
reduceZombieHp (coords,(Vaulting hp s)) amount = (coords, (Vaulting (hp-amount) s))