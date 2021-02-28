library(spotifyr)
library(genius)
library(geniusr)

taylor_df$track_name



info <- search_spotify("Taylor Swift", "artist")
info$id


disc_taylor <- get_discography("Taylor Swift")

disc_taylor$track_name

#Check the count of Taylor in each DF

spotify6 %>%
  dplyr::filter(artists %in% str_subset(spotify6$artists, "Taylor Swift")) %>%
  dplyr::distinct(name)
101

#Spotify 4
spotify4 %>%
  dplyr::filter(artists == "['Taylor Swift']") %>%
  dplyr::distinct(name)
126

spotify4_tay <- spotify4 %>%
  dplyr::filter(artists %in% str_subset(spotify4$artists, "Taylor Swift")) %>%
  dplyr::distinct(name)
139

#Spotify 5
spotify5 %>%
  dplyr::filter(artists == "['Taylor Swift']") %>%
  dplyr::distinct(name)
145

spotify5_test <- spotify5 %>%
  dplyr::filter(artists %in% str_subset(spotify5$artists, "Taylor Swift")) %>%
  dplyr::distinct(name)
159

spotify5_test
dplyr::union(spotify4_test)

dplyr::union_all(spotify5_test,spotify4_test) %>%
  dplyr::distinct()


taylor_filtered_test <- taylor_filtered %>%
  dplyr::distinct(track_name)

dplyr::intersect(spotify4_test, taylor_filtered_test)

dplyr::intersect(spotify4_test$name, taylor_filtered_test$track_name) 106
dplyr::intersect(spotify5_test$name, taylor_filtered_test$track_name) 131
##
spotify4_tay <- spotify4 %>%
  dplyr::filter(artists %in% str_subset(spotify4$artists, "Taylor Swift"))

spotify5_tay <- spotify5 %>%
  dplyr::filter(artists %in% str_subset(spotify5$artists, "Taylor Swift"))

spotify4_5_tay <- bind_rows(spotify4_tay,spotify5_tay)

spotify4_5_tay %>%
  dplyr::distinct(artists)

#STR REPLACE


spotify4_5_tay <- spotify4_5_tay %>%
  dplyr::mutate(artists = str_replace_all(artists, "\\'|\\[|\\]", ""))
  

spotify4_5_tay %>%
  dplyr::relocate(artists, .before = acousticness) %>%
  dplyr::relocate(name, .before = artists)


#Don't have the album name, so you will need to get that from

##
swift1 <- spotify4 %>%
  dplyr::filter(artists == "['Taylor Swift']")

swift1 <- swift1 %>%
  dplyr::select(artists, popularity, name)
##

swift2 <- spotify5 %>%
  dplyr::filter(artists == "['Taylor Swift']")

swift2 <- swift2 %>%
  dplyr::select(artists, popularity, name)
  
##

  
  tay_id <- "06HL4z0CvFAxyc27GXpf02"

  
sub6 <- str_subset(spotify6$artists, "Taylor Swift")
  
swift4 <- spotify6 %>%
    dplyr::filter(artists %in% sub6)
  
swift4 <- swift4 %>%
  dplyr::select(name, popularity, album.name)

swift4 <- swift4 %>%
  dplyr::rename(artists=name) %>%
  dplyr::rename(name=album.name)

swift_bind <- rbind(swift1, swift2, swift4)

swift_bind %>%
  dplyr::count(name)

swift_bind1 <- swift_bind %>%
  dplyr::count(name)

swift_bind1 <- swift_bind %>%
  dplyr::distinct(name, .keep_all = TRUE)

swift_bind1 <- swift_bind1 %>%
  dplyr::filter(!grepl(remove_list, name))

swift_bind1 <- slice(swift_bind1, -(149:161))


songs_i_want <- taylor_filtered$track_name 


swift_bind2 <- swift_bind1 %>%
  dplyr::filter(name %in% songs_i_want)

songs_i_have <- swift_bind2$name

filter(!grepl(songs_i_have, track_name))

taylor_filtered %>%
  filter(!grepl(songs_i_have, track_name))

songs_i_have %in% songs_i_want

songs_i_want %in% songs_i_have

dplyr::filter(songs_i_have %in% songs_i_want)
dplyr::filter(songs_i_want %in% songs_i_have)

taylor_filtered %>%
  dplyr::filter(songs_i_have %in% track_name)

songs_list <- vctrs::vec_c(songs_i_have, songs_i_want)

songs_list1 <- songs_list %>%
  as_tibble()


songs_list1 <- songs_list1 %>%
  dplyr::count(value)

missing_songs <- songs_list1 %>%
  dplyr::arrange(n) %>%
  top_n(-10) %>%
  dplyr::select(value)

missing_songs


dplyr::filter(missing_songs %in% name)
dplyr::filter(name %in% missing_songs)

spotify_songs %>%
  dplyr::filter(missing_songs %in% track_name)

dplyr::filter(track_name %in% missing_songs)


missing_songs



tay_playlist <- get_playlist_audio_features("b7iflt2s6um28zx952r2rp1q9", "5MhycXBWZlAbAKSot7d0Xe")

swift_bind2

tay_playlist1 <- tay_playlist %>%
  dplyr::select(track.name, track.popularity) %>%
  dplyr::mutate(artists = "Taylor Swift")

swift_bind2 %>%
  dplyr::mutate(across("artists", str_replace, "['Taylor Swift']", "Taylor Swift"))

swift_bind2 %>%
  dplyr::mutate(across("artists", str_replace, "[']", ""))

swift_bind2 %>%
  dplyr::mutate(across("artists", str_replace_all, ""\\[|\\]"", ""))


bad_char <- "['Taylor Swift']" 
bad_char1 <- paste(c("['Taylor Swift']"))

#Temp solution

swift_bind2 <- swift_bind2 %>%
  dplyr::select(-artists) %>%
  dplyr::mutate(artists = "Taylor Swift") %>%
  dplyr::relocate(artists, .before = popularity) %>%
  dplyr::relocate(popularity, .after = name)
  
tay_playlist1 <- tay_playlist1 %>%
  dplyr::rename(name = track.name) %>%
  dplyr::rename(popularity = track.popularity) %>%
  dplyr::relocate(artists, .before = name) %>%
  dplyr::relocate(popularity, .after = name)
  
  
tay_maybe <- rbind(swift_bind2, tay_playlist1)

dim(tay_maybe)

#You got all the songs pop into one DF. Now you want to redo it but have all the data lol