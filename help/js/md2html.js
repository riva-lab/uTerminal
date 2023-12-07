/**
 * Simple Markdown2HTML end-mile converter. Ready to actual use, no extra coding required.
 * Created by Riva, 2023-11-24.
 * Based on "Showdown" by ShowdownJS team https://github.com/showdownjs/showdown
 * Inspired by "markdown page" by Oscar Morrison https://github.com/oscarmorrison/md-page
 * MIT License.
 */

document.addEventListener("DOMContentLoaded", function () {
    // setPageDefaultStyle(); // Styles  
    setPageViewport();     // Viewport
    setPageEncoding();     // Encoding: utf-8
    updateColorScheme();

    switch (window.location.protocol) {

        // use this branch to load md content from external file
        case 'http:':
        case 'https:':
            loadContentExternal();
            break;

        // use this branch to load md content directly from template html file
        default:
            loadContentInternal();
    }
});


function setPageDefaultStyle() {
    var sheet = document.createElement('style');
    var styles = 'body { padding: 20px; font-family:  "Helvetica Neue", Helvetica, Arial, sans-serif;} ';
    styles += 'blockquote { padding: 0 1em; color: #6a737d; border-left: 0.25em solid #dfe2e5;} ';
    styles += 'code { padding: 0.2em 0.4em; background: rgba(27,31,35,0.05); border-radius: 3px;} ';
    styles += 'pre > code { background: none } ';
    styles += 'pre { padding: 16px; overflow: auto; line-height: 1.45; background-color: #f6f8fa; border-radius: 3px; } ';
    styles += 'table { border-collapse: collapse; } ';
    styles += 'td, th {  border: 1px solid #ddd; padding: 10px 13px; } ';
    sheet.innerHTML = styles;
    document.head.prepend(sheet);
}

function setPageViewport() {
    var viewportMeta = document.createElement('meta');
    viewportMeta.setAttribute('name', 'viewport');
    viewportMeta.setAttribute('content', 'width=device-width, initial-scale=1, shrink-to-fit=no');
    document.head.appendChild(viewportMeta);
}

function setPageEncoding() {
    var meta = document.createElement('meta');
    meta.setAttribute('charset', 'UTF-8');
    document.head.appendChild(meta);
}

function getMarkdownFilename() {
    var linkTag = document.getElementsByTagName('link');
    for (var i = 0; i < linkTag.length; i++)
        if (linkTag[i].getAttribute('rel') == 'markdown')
            return linkTag[i].getAttribute('href');

    return '';
}

function loadContentExternal() {
    var client = new XMLHttpRequest();
    client.open('GET', getMarkdownFilename());
    client.send();

    client.onload = function () {
        convertMarkdownToHtml(client.responseText);
    }

    client.onerror = function () {
        loadContentInternal();
    }
}

function loadContentInternal() {
    convertMarkdownToHtml(document.querySelector('noscript').innerText);
}

function convertMarkdownToHtml(markdown) {
    var converter = new showdown.Converter({
        emoji: true,
        underline: true,
    });
    converter.setFlavor('github');

    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /<a\shref[^>]+>/g,
            replace: function (text) {
                var url = text.match(/"(.*?)"/)[1];
                if (url.includes(window.location.hostname) || url[0] == '/' || url[0] == '.' || url[0] == '#') {
                    return text;
                }
                return '<a href="' + url + '" target="_blank">';
            }
        }];
    }, 'externalLink');

    // converter.addExtension(function () {
    //     return [{
    //         type: 'output',
    //         regex: /(<img\s[^>]*src="([^>"]*)"[^>]*>)/gi,
    //         replace: '<a href="$2" target="_blank">$1</a>'
    //     }];
    // }, 'imgLinkify');

    /**
     * Extension for converter to fix SVG rendering.
     * This code replace tag <img> with <embed> for .svg images.
     * This is necessary because of incorrect rendering png embedded in svg.
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<)(img)([^>]*src="[^"]*?screenshots\/[^"]*?.svg"[^>]*>)/gi,
            replace: '$1embed$3'
        }];
    }, 'fixSvg');

    /**
     * Simple XSS filter.
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /=\s*?"(javascript:[^"]*)"/gi,
            replace: '=""'
        }];
    }, 'preventJSExec');

    /**
     * IMG and EMBED tags wrapper
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /<p>(\s*?<(img|embed)\s.*?src="([^"]*?)".*?)<\/p>/gi,
            replace: '<a href="$3" target="_blank"><p class="p-$2">$1</p></a>'
        }];
    }, 'tagPbeforeImgOrEmbed');

    /**
     * TOC Generator
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            filter: function (text, converter, options) {
                return makeToc(text);
            }
        }];
    }, 'generateTOC');

    /**
     * ToDo checkboxes fix - trim beginning spaces
     */
    converter.addExtension(function () {
        return [{
            type: 'output',
            regex: /(<input[^>]*>)\s*/gi,
            replace: '$1'
        }];
    }, 'todoFix');

    document.body.innerHTML = converter.makeHtml(markdown) + document.body.innerHTML;
    document.title = document.title || document.body.firstElementChild.innerText.trim();

    updateColorScheme();
}

/**
 * Color scheme handling.
 */

function getImagesCatalog(type) {
    var linkTag = document.getElementsByTagName('link');
    for (var i = 0; i < linkTag.length; i++)
        if (linkTag[i].getAttribute('rel') == 'catalog')
            if (linkTag[i].getAttribute('type') == type)
                return linkTag[i].getAttribute('href');

    return '';
}

// https://stackoverflow.com/questions/56393880/how-do-i-detect-dark-mode-using-javascript
function setColorScheme(scheme) {
    var catDst = '';
    var catSrc = '';

    switch (scheme) {
        case 'dark':
            catDst = getImagesCatalog('dark');
            catSrc = getImagesCatalog('light');
            break;

        // case 'light': break;
        default:
            catDst = getImagesCatalog('light');
            catSrc = getImagesCatalog('dark');
            break;
    }

    // change images source
    var re = RegExp('src="' + catSrc + '([^>"]*?)"', 'gi');
    var str = 'src="' + catDst + '$1"';
    document.body.innerHTML = document.body.innerHTML.replace(re, str);

    // change links reference
    re = RegExp('<a href="' + catSrc + '([^>"]*?)"', 'gi');
    str = '<a href="' + catDst + '$1"';
    document.body.innerHTML = document.body.innerHTML.replace(re, str);
}

function getPreferredColorScheme() {
    if (window.matchMedia) {
        if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
            return 'dark';
        } else {
            return 'light';
        }
    }
    return 'light';
}

function updateColorScheme() {
    setColorScheme(getPreferredColorScheme());
}

if (window.matchMedia) {
    var colorSchemeQuery = window.matchMedia('(prefers-color-scheme: dark)');
    colorSchemeQuery.addEventListener('change', updateColorScheme);
}


/**
 * TOC handling: generating Table of Content.
 */

function tocListOpen(level) {
    return '<ul class="toc-h' + level + '">';
}

function tocListClose() {
    return '</ul>';
}

function tocListItemOpen(level) {
    return '<li class="toc-h' + level + '">';
}

function tocListItemClose() {
    return '</li>';
}

function makeToc(html) {
    var re = /(<)(h(\d))(\s[^>]*id="([^>"]*?)"[^>]*>)(.*?)(<\/h\3>)/gi;
    var headerTags = String(html).match(re);

    var str = '';
    var prevLevel = 0;

    if (headerTags != null && headerTags.length) {
        for (let h of headerTags) {
            var level = parseInt(h.replace(re, '$3'));

            if (level == prevLevel) {
                str += tocListItemClose() + tocListItemOpen(level);
            }
            else
                if (level > prevLevel) {
                    while (level > prevLevel++) str += tocListOpen(level);
                    str += tocListItemOpen(level);
                } else
                    if (level < prevLevel) {
                        str += tocListItemClose();
                        while (level < prevLevel--) str += tocListClose();
                        str += tocListItemOpen(level);
                    }

            str += h.replace(re, '<a href="#$5">$6</a>');
            prevLevel = level;
        }

        str += tocListItemClose();
        while (prevLevel--) str += tocListClose();
    }

    // make link to TOC on each header 
    re = /((<h(\d)\s[^>]*>)(.*?)(<\/h\3>))/gi;
    html = html.replace(re, '$2<a href="#toc">$4</a>$5');

    // insert TOC and return
    re = /<p>\s*\[toc\]\s*<\/p>/gi;
    str = '<div id="toc" class="toc">' + str + '</div>';
    return html.replace(re, '?#?#toc').replace('?#?#toc', str);
}