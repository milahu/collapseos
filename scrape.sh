#!/bin/sh

if [[ -d collapseos.org ]]; then
  echo "error: output dir exists: collapseos.org. fix: rm -rf collapseos.org"
  exit 1
fi

# init output dir from old files
# TODO if not exist, create orphan branches
[ -d website ] || git worktree add website website
[ -d website-files ] || git worktree add website-files website-files
[ -d website-md ] || git worktree add website-md website-md

# copy files as hardlinks
cp -r --link website collapseos.org
cp --link website-files/files/*.tar.gz collapseos.org/files/
rm -rf collapseos.org/.git
rm collapseos.org/.gitignore
rm collapseos.org/.nojekyll
# restore mtimes of files
declare -A mtimes
while read line
do
	mtime="$(echo "$line" | cut -d' ' -f1-3)"
	fpath="$(echo "$line" | cut -d' ' -f4-)"
	touch -m -d "$mtime" "$fpath"
done < <(cat collapseos.org/.mtimes)

# --mirror == -r -N -l inf --no-remove-listing
# --no-if-modified-since: fix: File ‘collapseos.org/index.html’ not modified on server. Omitting download.
# --server-response: preserve last-modified time of files https://superuser.com/questions/1734429/how-to-dowload-files-with-original-timestamp

# dont use "--convert-links" and "--convert-links" because they change mtime
#wget --mirror --convert-links --page-requisites --adjust-extension --no-if-modified-since --server-response http://collapseos.org/

echo "fetching files"
wget --mirror --page-requisites --no-if-modified-since --server-response http://collapseos.org/

echo "saving mtimes of files"
# save mtimes of files. mtimes are not preserved by git
# collapseos.org/files/index.html is a dynamic file with relative "Last modified" times like "550.8 days"
# we use the exact absolute mtimes from the server response headers, see also "curl -I"
find collapseos.org/ -type f | grep -v -x -F -e collapseos.org/files/index.html -e collapseos.org/.mtimes -e collapseos.org/.git -e collapseos.org/.gitignore | sort | while read f; do echo $(TZ=GMT stat -c%y $f) "$f"; done >collapseos.org/.mtimes

echo "patching mtimes in collapseos.org/files/index.html"
# patch /files/index.html: convert relative mtimes to absolute mtimes
# a: <td class="cmod">550.8 days</td>
# b: <td class="cmod">2022-03-18 21:39:26</td>
declare -A mtimes
while read line
do
	mtime="$(echo "$line" | cut -d' ' -f1-2 | sed 's/\.000000000$//')"
	cname="$(echo "$line" | cut -d' ' -f4- | sed 's|^collapseos.org/files/||')"
	mtimes["$cname"]="$mtime"
done < <(cat collapseos.org/.mtimes)
mtime=""
while read line
do
	if echo "$line" | grep -q '^<td class="cname">'; then
		cname=$(echo "$line" | cut -d '"' -f4)
		mtime=${mtimes[$cname]}
		echo "$line"
	elif echo "$line" | grep -q '^<td class="cmod">'; then
		echo "<td class=\"cmod\">$mtime</td>"
		mtime=""
	else
		echo "$line"
	fi
done < <(cat collapseos.org/files/index.html) | sponge collapseos.org/files/index.html
# fix: sponge does not preserve the hardlink, but creates a new file
cp --link --force collapseos.org/files/index.html website/files/index.html

echo "patching table style of collapseos.org/files/index.html"
wget https://www.fastmailusercontent.com/filestorage/css/dirlist.css -O collapseos.org/css/dirlist.css
sed -i 's/.cmod{width:15%}/.cmod{width:22%}/' collapseos.org/css/dirlist.css
sed -i 's|https://www.fastmailusercontent.com/filestorage/css/dirlist.css|../css/dirlist.css|' collapseos.org/files/index.html

echo "patching table script of collapseos.org/files/index.html"
mkdir -p collapseos.org/js
wget https://www.fastmailusercontent.com/filestorage/js/sorttable.js -O collapseos.org/js/sorttable.js
sed -i 's|https://www.fastmailusercontent.com/filestorage/js/sorttable.js|../js/sorttable.js|' collapseos.org/files/index.html

echo "patching links to index.html"
# a: <a href="/">
# b: <a href="./">
find collapseos.org/ -type f -name '*.html' -print0 | xargs -0 sed -i 's|<a href="/">|<a href="index.html">|'

echo "fixing broken html"
# example: <th>...</td> in collapseos.org/files/index.html

export PATH=$PATH:/nix/store/pkrrqgi79gfhdh4ka9wd4hhyz9k4qyz5-html-tidy-5.8.0/bin

# no. tidy is too invasive
# TODO find a "minimal diff" html fixer
if false; then
find collapseos.org/ -name '*.html' | while read f
do
	# this is a minimal tidy config
	# see also https://github.com/htacg/tidy-html5/issues/1055
	# https://tidy.sourceforge.net/docs/tidy_man.html
	tidy --tidy-mark no --quiet yes --indent no --wrap 0 --force-output yes \
	--markup yes --doctype user --add-xml-decl no --break-before-br no \
	--preserve-entities yes --keep-time yes \
	--show-warnings no --show-errors 0 --newline LF --write-back yes $f
done
else
# fix known html errors
# fix broken th tag, otherwise pandoc fails to convert table to markdown
# a: <th>...</td>
# b: <th>...</th>
sed -i 's|<th id="hrownum" class="crownum">Row</td>|<th id="hrownum" class="crownum">Row</th>|' collapseos.org/files/index.html
# remove unexpected </a>
sed -i '/^<\/a>$/d' collapseos.org/why.html
# use tidy to find more html errors
echo "validating html"
find collapseos.org/ -name '*.html' | while read f
do
	tidy --quiet yes --markup no --show-warnings yes --show-errors 6 $f 2>&1 | grep -v 'not allowed for HTML5$' | sed "s|^|$f: |"
done
fi

# apply changes back to website/ and website-files/
#cp --link --force collapseos.org/.mtimes website/.mtimes
# add/modify files
shopt -s dotglob
cp --link --force -r collapseos.org/* website/
cp --link --force -r collapseos.org/* website-files/
# remove files
for worktree_path in website/ website-files/; do
	while read path
	do
		# ignore special files
		if [[ "$path" == ".git" ]]; then continue; fi
		if [[ "$path" == ".gitignore" ]]; then continue; fi
		if [[ "$path" == ".nojekyll" ]]; then continue; fi
		if ! [ -e "collapseos.org/$path" ]; then
			echo "removing file from git: $worktree_path/$path"
			git -C "$worktree_path" rm "$path"
		fi
	done < <(cd "$worktree_path" && find . -type f -printf "%P\n")
done
# disable jekyll on github pages
touch website/.nojekyll

echo "converting html to markdown"
git -C website-md/ rm -rf .
find website -type f -name '*.html' | while read html_path
do
	md_path=${html_path%.html}.md
	md_path=website-md/${md_path#website/}
	mkdir -p $(dirname $md_path)
	echo creating $md_path
	# grid_tables are ugly... just use html tables
	# also, grid_tables fails on hardware.html
	#pandoc -f html -t markdown_strict+grid_tables --wrap=none $html_path -o $md_path
	pandoc -f html -t markdown_strict --wrap=none $html_path -o $md_path
done
find website-md/ -name index.md | while read f
do
	d=$(dirname $f)
	#mkdir -p $d/.github; ln -s ../index.md $d/.github/readme.md
	mv $f $d/readme.md
done
# patch internal links from *.html to *.md
# grep -r -E 'href=".*html"' website | grep -v '"http'
sed_script=''
sed_script+='s|\]\(index\.html(#[^) ]+)?\)|](readme.md\1)|g;'$'\n'
sed_script+='s|href="index\.html(#[^) ]+)?"|href="readme.md\1"|g;'$'\n'
# collect potential link targets
# this is simple, because all files are in the root folder
while read f
do
	html_path=${f#website/}
	if [[ "$html_path" == "index.html" ]]; then continue; fi
	md_path=${html_path%.html}.md
	html_path_esc=$(echo "$html_path" | sed 's/\./\\./g')
	# note: also convert html anchor links: why.html#creative -> why.md#creative
	sed_script+='s|\]\('$html_path_esc'(#[^) ]+)?\)|]('$md_path'\1)|g;'$'\n'
	sed_script+='s|href="'$html_path_esc'(#[^) ]+)?"|href="'$md_path'\1"|g;'$'\n'
done < <(find website/ -name '*.html')
find website-md/ -name '*.md' | while read f
do
	sed -i -E "$sed_script" $f
done

# commit changes
for worktree_path in website/ website-files/ website-md/; do
	git -C "$worktree_path" commit -m update
done

# rebuild the main branch
git branch main2
git worktree add main2/ main2
if false; then
	# delete all commits after the "Last git commit" on Tue Jan 5 11:34:27 2021 -0500
	git -C main2/ reset --hard 4da8b686237596fe36cc76649fbc09089b60f91f
	git tag 2021-01-05-git 4da8b686237596fe36cc76649fbc09089b60f91f
fi
# last git tag. example: 2023-04-27
last_git_tag=$(git tag -l | sort | grep -E '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' | tail -n1)
# last tar name. example: collapseos-20230427.tar.gz
last_tar_name=collapseos-$(echo $last_git_tag | tr -d -).tar.gz
# loop the tarballs
# collapseos-git-20210105.tar.gz: all git commits until 4da8b686237596fe36cc76649fbc09089b60f91f
# cosuxn.tar.gz: between 2021-08-13 and 2021-08-31
# collapseos-20210105.tar.gz: first tarball. should be identical to collapseos-git-20210105.tar.gz
while read tar_path
do
	echo "tar_path: $tar_path"
	tar_name=$(basename $tar_path)
	tar_base=${tar_name%.tar.gz}
	tar_date=$(echo ${tar_base#collapseos-} | sed -E 's/^(.{4})(.{2})(.{2})/\1-\2-\3/')
	git -C main2/ rm --quiet -rf .
	tar xf $tar_path -C main2/
	git -C main2/ add .
	commit_message=""
	seen_first_header=false
	while read line; do
		if [[ "${line:0:6}" == "*** 20" ]]; then
			$seen_first_header && break
			seen_first_header=true
			continue
		fi
		$seen_first_header || continue
		commit_message+="$line"$'\n'
	done < <(cat main2/CHANGES)
	#commit_message=$(echo "$commit_message" | tail -n +2) # remove first line: *** 2021-05-15
	# | sed '1,/^$/d' # remove first empty lines
	d="$tar_date"T00:00:00Z-0500
	an="Virgil Dupras"
	ae="hsoft@hardcoded.net"
	GIT_AUTHOR_NAME="$an" \
	GIT_AUTHOR_EMAIL="$ae" \
	GIT_COMMITTER_NAME="$an" \
	GIT_COMMITTER_EMAIL="$ae" \
	GIT_AUTHOR_DATE="$d" \
	GIT_COMMITTER_DATE="$d" \
	git -C main2/ commit -m "$commit_message" --quiet
	new_commit_hash=$(git -C main2/ rev-parse HEAD)
	git tag -d $tar_date.tar.gz >/dev/null 2>&1
	git tag -d $tar_date >/dev/null 2>&1
	git tag $tar_date $new_commit_hash
done < <(
	find collapseos.org/files/ -name '*.tar.gz' |
	grep -v -x \
		-e collapseos.org/files/cosuxn.tar.gz \
		-e collapseos.org/files/collapseos-git-20210105.tar.gz |
	sort |
	grep -A999999 -x -F collapseos.org/files/$last_tar_name |
	tail -n +2
)
