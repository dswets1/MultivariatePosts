
# getting Spotify access information
library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = '8604c19e32d649869e03f09e167b8f01')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b9b41901631f477aa989d5ffeecbc2a6')
access_token <- get_spotify_access_token()


# assembling songs from playlists into big playlist
# getting overall playlist info
playlists <- get_user_playlists("1210694053")

# getting audio features for each playlist
tracks <- get_playlist_audio_features(username = "1210694053", playlist_uris = playlists$playlist_uri)

# extracting only the numeric variables
num_track_features <- tracks[, unlist(lapply(tracks, is.numeric))]

# removing the first two and last 2 columns from this as these columns are not really approproiate/relevant for comparison
num_track_features <- num_track_features[, -c(1:2, 12:13)]

# scaling variables in preparation for clustering
scale_num_track_features <- data.frame(scale(num_track_features))


fit <- kmeans(scale_num_track_features, 3)


# let's do some clustering
SSEs <- rep(NA,10) # a vector to store SSEs for different k's
SSEs[1] <- fit$totss # total SSE if no clustering is done
for(k in 2:10){
  fit <- kmeans(scale_num_track_features,k)
  SSEs[k] <- fit$tot.withinss
}
par(mar=c(4,4,1,1))
plot(1:10,SSEs,type="b",xlab="Number of Clusters")


# we'll go with 4 overall clusters distinct clusters, since adding additional ones 
# has the potential to convolute results and stops adding value after 4  
fit <- kmeans(scale_num_track_features, 4)



# pulling raw values based on cluster results to ease in comparison interpretation


cluster_info <- data.frame(num_track_features, fit$cluster)

(cluster_means <- aggregate(cluster_info, list(cluster_info$fit.cluster), mean)[,-1])

