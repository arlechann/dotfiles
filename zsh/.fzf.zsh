# Setup fzf

if [[ ! "$PATH" == */home/arle/.fzf/bin* ]] && [ ! -f "$(which fzf)" ]; then
  export PATH="${PATH:+${PATH}:}/home/arle/.fzf/bin"
fi

# Auto-completion

if [[ $- == *i* ]]; then
	[ -e "$HOME/.fzf/shell/completion.zsh" ] && source "$HOME/.fzf/shell/completion.zsh" 2> /dev/null
	[ -e "/usr/share/fzf/completion.zsh" ] && source "/usr/share/fzf/completion.zsh"
fi

export FZF_COMPLETION_OPTS='-m'

# Key bindings

#source "/home/arle/.fzf/shell/key-bindings.zsh"

# Functions

## depending packages

dependencies() {
	local list=""
	[ ! -f "$(which unbuffer)" ] && list="$list\"expect\" "
	[ "$list" != "" ] && echo -e "Please install next packages \n$list" 1>&2
}

dependencies

## shell utils

fhistory() {
	history -r 1 | fzf -m --no-sort $@ | cut -d' ' -f4-
}

fhist() {
	fhistory | zsh
}

fuzzyfind() {
	find . | sed 's@^\./@@' | fzf $@
}

fdir() {
	find . -type d | sed 's@^\./@@' | fzf --preview "ls --color {}" $@
}

ffile() {
	find . -type f | sed 's@^\./@@' |  fzf --preview "bat -n --color=always --paging=never {}" $@
}

fcd() {
	cd "$(fdir)"
}

fls() {
	ls -lah $1 | fzf -m --header-lines=1 --preview "bat -n --color=always --paging=never $1/{+9}" | awk '$0=$9'
}

frm() {
	fuzzyfind | xargs -r rm $@
}

fps() {
	ps aux | fzf -m --no-sort --header-lines=1 | awk '$0=$2'
}

fkill() {
	fps | xargs -r kill $1
}

fvim() {
	local file=$(ffile)
	[ -n "$file" ] && vim "$file"
}

fopen() {
	ffile -m | xargs -r xdg-open
}

## systemctl

fsyslist() {
	systemctl list-unit-files -a --no-pager | head -n-7 | fzf --with-nth=1 --header-lines=1 --preview "unbuffer systemctl status --no-pager | sed '/^$/,/$$/d'" $@ | awk '$0=$1'
}

fsysstatus() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && systemctl status "$service" $@
}

fsysstart() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl start "$service"
}

fsysstop() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl stop "$service"
}

fsysrestart() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl restart "$service"
}

fsyskill() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl kill "$service"
}

fsysenable() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl enable "$service"
}

fsysdisable() {
	local service=$(fsyslist -m)
	[ -n "$service" ] && sudo systemctl disable "$service"
}

## docker

fdimages() {
	local image=$(sudo docker images)
	echo "$images" | fzf --header-lines=1 $@ | awk '$0=$3'
}

fdrmi() {
	fdimages -m | xargs -r sudo docker rmi
}

fdps() {
	local ps=$(sudo docker ps -a)
	echo "$ps" | fzf --header-lines=1 $@ | awk '$0=$1'
}

fdstop() {
	fdps -m | xargs -r sudo docker stop
}

fdrm() {
	fdps -m | xargs -r sudo docker rm
}

fdexec() {
	local container_id=$(fdps)
	[ -n "$container_id" ] && sudo docker exec -it "$container_id" "${@:-/bin/bash}"
}

fdcp() {
	local help="Usage: fdcp <command>

command
	pull	copy from container to host
	push	copy from host to container"

	if [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
		echo $help
		return 1
	fi

	if [ "$1" == "pull" ]; then
		local container_id=$(fdps)
		local container_file=$(sudo docker exec $container_id find / | fzf --header="container file")
		local host_dir=$(fdir --header="host directory")

		[ -n "$container_id" ] && [ -n "$container_file" ] && [ -n "$host_dir" ] && sudo docker cp "$container_id:$container_file" "$host_dir"
	elif [ "$1" ==  "push" ]; then
		local container_id=$(fdps)
		local host_file=$(fzf --header="host file")
		local container_dir=$(sudo docker exec "$container_id" find / | fzf --header="container dir")

		[ -n "$container_id" ] && [ -n "$container_dir" ] && [ -n "$host_file" ] && sudo docker cp "$host_file" "$container_id:$container_dir"
	else
		echo $help
	fi
}

