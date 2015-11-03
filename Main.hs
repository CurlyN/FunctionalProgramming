-- file: ch22/PodDB.hs
module DB where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes
import Control.Monad(when)
import Data.List(sort)


{- | Prepare the database for our data.

We create two tables and ask the database engine to verify some pieces
of data consistency for us:

* castid and epid both are unique primary keys and must never be duplicated
* castURL also is unique
* In the episodes table, for a given podcast (epcast), there must be only
  one instance of each given URL or episode ID
-}
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("games" `elem` tables)) $
           do run dbh "CREATE TABLE games (\
                       \gameid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
					   \description TEXT NOT NULL,\
					   \name TEXT NOT NULL,\
                       \date_added TEXT NOT NULL, \
					   \)" []
              return ()
       when (not ("raitings" `elem` tables)) $
           do run dbh "CREATE TABLE raitings (\
                       \rid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                       \raiting_id INTEGER NOT NULL,\
                       \g_name TEXT NOT NULL,\
                       \)" []
              return ()
       commit dbh

{- | Adds a new podcast to the database.  Ignores the castid on the
incoming podcast, and returns a new object with the castid populated.

An attempt to add a podcast that already exists is an error. -}
addDatame :: Game -> IO String
addGame dbh podcast = 
    handleSql errorHandler $
      do -- Insert the castURL into the table.  The database
         -- will automatically assign a cast ID.
         d <- connectSqlite3 "test.db"
		prepDB d
		check <- getGameData d $ Download.id bO
		case check of
			[] -> (run d "INSERT INTO games (gameid, description, name, date_added) VALUES (?,?,?,?)"
				[toSql (Download.id bO), toSql (Download.description bO),
					toSql (AKDownload.name bO), toSql (AKDownload.date_added bO)])
			[x] -> (run d "UPDATE games SET description = ?, name = ?, date_added = ? WHERE gameid = ?"
				[toSql (AKDownload.description bO), toSql (AKDownload.name bO),
					toSql (AKDownload.date_added bO), toSql (AKDownload.id bO)])
		mapM (addGameData d (Download.id bO)) $ AKDownload.abridged_cast bO
		commit d
		disconnect d
		return $ Download.name bO
	where errorHandler e =
		do fail $ "Error adding data;\n" ++ show e

{- | Adds a new episode to the database. 

Since this is done by automation, instead of by user request, we will
simply ignore requests to add duplicate episodes.  This way, when we are
processing a feed, each URL encountered can be fed to this function,
without having to first look it up in the DB.

Also, we generally won't care about the new ID here, so don't bother
fetching it. -}
addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epCastId, epURL, epDone) \
                \VALUES (?, ?, ?)"
                [toSql (castId . epCast $ ep), toSql (epURL ep),
                 toSql (epDone ep)]
    >> return ()
       
{- | Modifies an existing podcast.  Looks up the given podcast by
ID and modifies the database record to match the passed Podcast. -}
updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
    run dbh "UPDATE podcasts SET castURL = ? WHERE castId = ?" 
            [toSql (castURL podcast), toSql (castId podcast)]
    >> return ()

{- | Modifies an existing episode.  Looks it up by ID and modifies the
database record to match the given episode. -}
updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode =
    run dbh "UPDATE episodes SET epCastId = ?, epURL = ?, epDone = ? \
             \WHERE epId = ?"
             [toSql (castId . epCast $ episode),
              toSql (epURL episode),
              toSql (epDone episode),
              toSql (epId episode)]
    >> return ()

{- | Remove a podcast.  First removes any episodes that may exist
for this podcast. -}
removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast =
    do run dbh "DELETE FROM episodes WHERE epcastid = ?" 
         [toSql (castId podcast)]
       run dbh "DELETE FROM podcasts WHERE castid = ?"
         [toSql (castId podcast)]
       return ()

{- | Gets a list of all podcasts. -}
getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbh =
    do res <- quickQuery' dbh 
              "SELECT castid, casturl FROM podcasts ORDER BY castid" []
       return (map convPodcastRow res)

{- | Get a particular podcast.  Nothing if the ID doesn't match, or
Just Podcast if it does. -}
getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbh wantedId =
    do res <- quickQuery' dbh 
              "SELECT castid, casturl FROM podcasts WHERE castid = ?"
              [toSql wantedId]
       case res of
         [x] -> return (Just (convPodcastRow x))
         [] -> return Nothing
         x -> fail $ "Really bad error; more than one podcast with ID"

{- | Convert the result of a SELECT into a Podcast record -}
convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svId, svURL] =
    Podcast {castId = fromSql svId,
             castURL = fromSql svURL}
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

{- | Get all episodes for a particular podcast. -}
getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes dbh pc =
    do r <- quickQuery' dbh
            "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?"
            [toSql (castId pc)]
       return (map convEpisodeRow r)
    where convEpisodeRow [svId, svURL, svDone] =
              Episode {epId = fromSql svId, epURL = fromSql svURL,
                       epDone = fromSql svDone, epCast = pc}