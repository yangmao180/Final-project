find_by_title(
  "Up",
  type = NULL,
  season = NULL,
  episode = NULL,
  year_of_release = NULL,
  plot = "short",
  include_tomatoes = FALSE,
  api_key = omdb_api_key()
)

api_key <- Sys.getenv("TMDB_API_KEY")

discover_movie(
  api_key = api_key,
  certification_country = NA,
  certification = NA,
  certification.lte = NA,
  include_adult = FALSE,
  include_video = TRUE,
  language = NA,
  page = 1,
  primary_release_year = NA,
  primary_release_date.gte = NA,
  primary_release_date.lte = "2022-01-01",
  release_date.gte = NA,
  release_date.lte = NA,
  sort_by = NA,
  vote_count.gte = NA,
  vote_count.lte = NA,
  vote_average.gte = NA,
  vote_average.lte = NA,
  with_cast = NA,
  with_crew = NA,
  with_companies = NA,
  with_genres = NA,
  with_keywords = NA,
  with_people = NA,
  year = NA
)
