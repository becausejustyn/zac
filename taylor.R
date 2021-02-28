

taylor_with_lyrics <- read_csv("~/Downloads/tayloR-master/taylor_with_lyrics.csv")


## Get data
taylor <- get_artist_audio_features('taylor swift')

# Reorder columns 
taylor <- taylor %>%
  dplyr::relocate(album_name, .before = artist_id) %>%
  dplyr::relocate(track_name, .before = artist_name)

#View the album names so you can remove the ones you don't want
taylor %>%
  dplyr::distinct(album_name)

#List to keep
album_list <- paste(c("Taylor Swift", "Fearless Platinum Edition", "Speak Now", "Red", "1989", "reputation", "Lover", "folklore", "evermore"))

#List of words in song titles to remove
remove_list <- paste(c("Demo Recording", "Karaoke Version", "Pop Version", "Acoustic Version", "Commentary", "Medley", "Instrumental", 
                       "Live From", "Edit", "Mix", "Radio", "Live/","Piano Version"), collapse = '|')

#Create tibble
taylor_filtered <- taylor %>%
  as_tibble()

#Remove albums you don't want
taylor_filtered <- taylor_filtered %>%
  dplyr::filter(album_name %in% album_list)

#Moving the columns around
taylor_filtered <- taylor_filtered %>%
  dplyr::relocate(track_name, .before = album_name)

#Removing songs
taylor_filtered <- taylor_filtered %>%
  filter(!grepl(remove_list, track_name))

#Removing duplicates
taylor_filtered <- taylor_filtered %>%
  dplyr::distinct(track_name, .keep_all = TRUE)


#Renaming the Fearless album
  
taylor_filtered <- taylor_filtered %>%
  dplyr::mutate(across("album_name", str_replace, "Fearless Platinum Edition", "Fearless"))
  

#check for missing data
sum(is.na(taylor_filtered))

#remove track_preview_url since we don't need it anyway
taylor_filtered <- taylor_filtered %>%
  dplyr::select(-track_preview_url)

sum(is.na(taylor_filtered))

taylor_df <- taylor_filtered %>%
  dplyr::select(artist_name, track_name, album_name, artist_id, album_id, track_id, analysis_url, track_uri)