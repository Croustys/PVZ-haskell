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


{- 
  performZombieActions (GameModel 0 [] [((0,0), coneHead)]) == Nothing
  performZombieActions (GameModel 0 [] [((0,1), coneHead)]) == Just (GameModel 0 [] [((0,0), coneHead)])
  performZombieActions (GameModel 0 [((0,1), defaultWalnut)] [((0,1), coneHead)]) == Just (GameModel 0 [((0,1), Walnut 14)] [((0,1), coneHead)])
  performZombieActions (GameModel 0 [((0,1), defaultWalnut)] [((0,1), vaulting)]) == Just (GameModel 0 [((0,1), defaultWalnut)] [((0,0), Vaulting 7 1)])

  performZombieActions (GameModel 0 [((4,2),Sunflower 5)] [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]])==Just (GameModel 0 [((4,2),Sunflower 2)] [((0,1),Conehead 10 1),((0,1),Basic 5 1),((0,1),Buckethead 20 1),((0,0),Vaulting 7 2),((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 2),((0,6),Conehead 10 1),((0,6),Basic 5 1),((0,6),Buckethead 20 1),((0,5),Vaulting 7 2),((0,10),Conehead 10 1),((0,10),Basic 5 1),((0,10),Buckethead 20 1),((0,9),Vaulting 7 2),((2,1),Conehead 10 1),((2,1),Basic 5 1),((2,1),Buckethead 20 1),((2,0),Vaulting 7 2),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 2),((2,6),Conehead 10 1),((2,6),Basic 5 1),((2,6),Buckethead 20 1),((2,5),Vaulting 7 2),((2,10),Conehead 10 1),((2,10),Basic 5 1),((2,10),Buckethead 20 1),((2,9),Vaulting 7 2),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,6),Conehead 10 1),((4,6),Basic 5 1),((4,6),Buckethead 20 1),((4,5),Vaulting 7 2),((4,10),Conehead 10 1),((4,10),Basic 5 1),((4,10),Buckethead 20 1),((4,9),Vaulting 7 2)])

  performZombieActions (GameModel 0 [((0,7),Sunflower 5)] [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]])==Just (GameModel 0 [((0,7),Sunflower 2)] [((0,1),Conehead 10 1),((0,1),Basic 5 1),((0,1),Buckethead 20 1),((0,0),Vaulting 7 2),((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 2),((0,7),Conehead 10 1),((0,7),Basic 5 1),((0,7),Buckethead 20 1),((0,6),Vaulting 7 1),((0,10),Conehead 10 1),((0,10),Basic 5 1),((0,10),Buckethead 20 1),((0,9),Vaulting 7 2),((2,1),Conehead 10 1),((2,1),Basic 5 1),((2,1),Buckethead 20 1),((2,0),Vaulting 7 2),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 2),((2,6),Conehead 10 1),((2,6),Basic 5 1),((2,6),Buckethead 20 1),((2,5),Vaulting 7 2),((2,10),Conehead 10 1),((2,10),Basic 5 1),((2,10),Buckethead 20 1),((2,9),Vaulting 7 2),((4,1),Conehead 10 1),((4,1),Basic 5 1),((4,1),Buckethead 20 1),((4,0),Vaulting 7 2),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 2),((4,6),Conehead 10 1),((4,6),Basic 5 1),((4,6),Buckethead 20 1),((4,5),Vaulting 7 2),((4,10),Conehead 10 1),((4,10),Basic 5 1),((4,10),Buckethead 20 1),((4,9),Vaulting 7 2)])

  performZombieActions (GameModel 0 [((a,b),c 10)|a<-[0,2,4],b<-[0,2,7,11],c<-[Sunflower,Peashooter,Walnut,CherryBomb]] [((a,b),c)|a<-[0,2,4],b<-[2,3,7,11],c<-[coneHead,basic,bucketHead,vaulting]])==Just (GameModel 0 [((0,0),Sunflower 10),((0,0),Peashooter 10),((0,0),Walnut 10),((0,0),CherryBomb 10),((0,2),Sunflower 7),((0,2),Peashooter 7),((0,2),Walnut 7),((0,2),CherryBomb 7),((0,7),Sunflower 7),((0,7),Peashooter 7),((0,7),Walnut 7),((0,7),CherryBomb 7),((0,11),Sunflower 7),((0,11),Peashooter 7),((0,11),Walnut 7),((0,11),CherryBomb 7),((2,0),Sunflower 10),((2,0),Peashooter 10),((2,0),Walnut 10),((2,0),CherryBomb 10),((2,2),Sunflower 7),((2,2),Peashooter 7),((2,2),Walnut 7),((2,2),CherryBomb 7),((2,7),Sunflower 7),((2,7),Peashooter 7),((2,7),Walnut 7),((2,7),CherryBomb 7),((2,11),Sunflower 7),((2,11),Peashooter 7),((2,11),Walnut 7),((2,11),CherryBomb 7),((4,0),Sunflower 10),((4,0),Peashooter 10),((4,0),Walnut 10),((4,0),CherryBomb 10),((4,2),Sunflower 7),((4,2),Peashooter 7),((4,2),Walnut 7),((4,2),CherryBomb 7),((4,7),Sunflower 7),((4,7),Peashooter 7),((4,7),Walnut 7),((4,7),CherryBomb 7),((4,11),Sunflower 7),((4,11),Peashooter 7),((4,11),Walnut 7),((4,11),CherryBomb 7)] [((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 1),((0,2),Conehead 10 1),((0,2),Basic 5 1),((0,2),Buckethead 20 1),((0,1),Vaulting 7 1),((0,7),Conehead 10 1),((0,7),Basic 5 1),((0,7),Buckethead 20 1),((0,6),Vaulting 7 1),((0,11),Conehead 10 1),((0,11),Basic 5 1),((0,11),Buckethead 20 1),((0,10),Vaulting 7 1),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 1),((2,2),Conehead 10 1),((2,2),Basic 5 1),((2,2),Buckethead 20 1),((2,1),Vaulting 7 1),((2,7),Conehead 10 1),((2,7),Basic 5 1),((2,7),Buckethead 20 1),((2,6),Vaulting 7 1),((2,11),Conehead 10 1),((2,11),Basic 5 1),((2,11),Buckethead 20 1),((2,10),Vaulting 7 1),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,2),Conehead 10 1),((4,2),Basic 5 1),((4,2),Buckethead 20 1),((4,1),Vaulting 7 1),((4,7),Conehead 10 1),((4,7),Basic 5 1),((4,7),Buckethead 20 1),((4,6),Vaulting 7 1),((4,11),Conehead 10 1),((4,11),Basic 5 1),((4,11),Buckethead 20 1),((4,10),Vaulting 7 1)])

-}

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions gm@(GameModel s [] zombies)
  | isGameOver gm = Nothing
  | otherwise = Just (GameModel s [] (moveZombies zombies))
performZombieActions (GameModel s plants@(currentPlant@((plantX, plantY), cp):restPlants) zombies@(currentZombie@((zombieX, zombieY), cz):restZombies))
  | filter (\y -> y == False) (map (\p -> isZombieAbleToMove currentZombie p) plants) == [] = Just (GameModel s plants (moveZombies zombies))
  | filter (\y -> y == False) (map (\p -> isZombieAbleToMove currentZombie p) plants) /= [] && (isVaultingZombie cz) = Just (GameModel s plants (moveZombiesVaulting zombies))
  | otherwise = Just (GameModel s (((plantX, plantY), reducePlantHp cp):restPlants) zombies)
performZombieActions _ = Nothing

isGameOver :: GameModel -> Bool
isGameOver (GameModel _ _ []) = False
isGameOver (GameModel s p (((_, y), _):rest))
  | y <= 0 = True
  | otherwise = isGameOver (GameModel s p rest)

isVaultingZombie :: Zombie -> Bool
isVaultingZombie (Vaulting _ _) = True
isVaultingZombie _ = False

isZombieAbleToMove :: (Coordinate, Zombie) -> (Coordinate, Plant) -> Bool
isZombieAbleToMove (zombieCoords@(zombieX, zombieY), z) (plantCoords@(plantX, plantY), _)
  | zombieCoords == plantCoords = False
  | (zombieCoords == plantCoords) && (isVaultingZombie z) = True
  | otherwise = True

moveZombie :: (Coordinate, Zombie) -> (Coordinate, Zombie)
moveZombie ((x,y), z) = ((x, (y-1)), z)

moveVaulting :: (Coordinate, Zombie) -> (Coordinate, Zombie)
moveVaulting ((x,y), (Vaulting hp speed))
  | speed > 1 = ((x, (y-1)), (Vaulting hp (speed-1)))
  | otherwise = ((x, (y-1)), (Vaulting hp speed))
moveVaulting ((x,y), z) = ((x, (y-1)), z)

moveZombies :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
moveZombies z = map (\x -> moveZombie x) z

moveZombiesVaulting :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
moveZombiesVaulting z = map (\x -> moveVaulting x) z

reducePlantHp :: Plant -> Plant
reducePlantHp (Peashooter hp) = (Peashooter (hp-1))
reducePlantHp (Sunflower hp) = (Sunflower (hp-1))
reducePlantHp (Walnut hp) = (Walnut (hp-1))
reducePlantHp (CherryBomb hp) = (CherryBomb (hp-1))




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
