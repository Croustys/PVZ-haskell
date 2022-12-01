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
defaultCherryBomb = CherryBomb 2

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
performZombieActions (GameModel s plants zombies) = Just (GameModel s (performPlants plants zombies) (performZombies (concat (map (\currentZombie -> map (\currentPlant -> isZombieAbleToMove currentZombie currentPlant) plants) zombies)) zombies))


test_concat = (map (\currentZombie -> map (\currentPlant -> isZombieAbleToMove currentZombie currentPlant) test_plants) test_zombies)
test_plants = [((a,b),c 10)|a<-[0,2,4],b<-[0,2,7,11],c<-[Sunflower,Peashooter,Walnut,CherryBomb]]
test_zombies = [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]]
test_length (x:xs) = length x

--test_performZombies = performZombies test_concat test_zombies

performZombies :: [Int] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
performZombies [] _ = []
performZombies _ [] = []
performZombies (cInt:rInts) (cZombie:rZombies)
  | cInt == 3 = (moveZombie cZombie) : performZombies rInts rZombies
  | cInt /= 0 = (moveVaulting cZombie cInt) : performZombies rInts rZombies
  | otherwise = cZombie : performZombies rInts rZombies

performPlants :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]
performPlants plants zombies = map (\plant -> performPlant plant zombies) plants

performPlant :: (Coordinate, Plant) -> [(Coordinate, Zombie)] -> (Coordinate, Plant)
performPlant plant zombies = reducePlantHp plant (length (filter (\each -> each == True) (map (\currentZombie -> (shouldZombieAttack currentZombie plant)) zombies)))


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



test_model = (GameModel 0 [((a,b),c 10)|a<-[0,2,4],b<-[0,2,7,11],c<-[Sunflower,Peashooter,Walnut,CherryBomb]] [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]])


test = performZombieActions (GameModel 0 [((a,b),c 10)|a<-[0,2,4],b<-[0,2,7,11],c<-[Sunflower,Peashooter,Walnut,CherryBomb]] [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]])==Just (GameModel 0 [((0,0),Sunflower 10),((0,0),Peashooter 10),((0,0),Walnut 10),((0,0),CherryBomb 10),((0,2),Sunflower 7),((0,2),Peashooter 7),((0,2),Walnut 7),((0,2),CherryBomb 7),((0,7),Sunflower 7),((0,7),Peashooter 7),((0,7),Walnut 7),((0,7),CherryBomb 7),((0,11),Sunflower 7),((0,11),Peashooter 7),((0,11),Walnut 7),((0,11),CherryBomb 7),((2,0),Sunflower 10),((2,0),Peashooter 10),((2,0),Walnut 10),((2,0),CherryBomb 10),((2,2),Sunflower 7),((2,2),Peashooter 7),((2,2),Walnut 7),((2,2),CherryBomb 7),((2,7),Sunflower 7),((2,7),Peashooter 7),((2,7),Walnut 7),((2,7),CherryBomb 7),((2,11),Sunflower 7),((2,11),Peashooter 7),((2,11),Walnut 7),((2,11),CherryBomb 7),((4,0),Sunflower 10),((4,0),Peashooter 10),((4,0),Walnut 10),((4,0),CherryBomb 10),((4,2),Sunflower 7),((4,2),Peashooter 7),((4,2),Walnut 7),((4,2),CherryBomb 7),((4,7),Sunflower 7),((4,7),Peashooter 7),((4,7),Walnut 7),((4,7),CherryBomb 7),((4,11),Sunflower 7),((4,11),Peashooter 7),((4,11),Walnut 7),((4,11),CherryBomb 7)] [((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 1),((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 1),((0,7),Conehead 10 1),((0,7),Basic 5 1),((0,7),Buckethead 20 1),((0,6),Vaulting 7 1),((0,11),Conehead 10 1),((0,11),Basic 5 1),((0,11),Buckethead 20 1),((0,10),Vaulting 7 1),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 1),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 1),((2,7),Conehead 10 1),((2,7),Basic 5 1),((2,7),Buckethead 20 1),((2,6),Vaulting 7 1),((2,11),Conehead 10 1),((2,11),Basic 5 1),((2,11),Buckethead 20 1),((2,10),Vaulting 7 1),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,7),Conehead 10 1),((4,7),Basic 5 1),((4,7),Buckethead 20 1),((4,6),Vaulting 7 1),((4,11),Conehead 10 1),((4,11),Basic 5 1),((4,11),Buckethead 20 1),((4,10),Vaulting 7 1)])


test_ = performZombieActions test_model




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
