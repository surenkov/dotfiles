alias yt-dl="podman run --rm -i -e PGID=$(id -g) -e PUID=$(id -u) -v $(pwd):/workdir:rw mikenye/youtube-dl"
