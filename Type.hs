module Types where

data Creator = Creator
			   {
				  creatorid::Integer, -- id of creator
				  title :: String,   -- title of the group
				  creator:: String   -- the name of the creator of the group
			   }
			   deriving (Eq, Show, Read)

data Group = Group
               {
                  groupid :: Integer,    -- Group id
                  name :: String,    -- name of the group
                  description :: String      -- description of the group
               }
               deriving (Eq, Show, Read)