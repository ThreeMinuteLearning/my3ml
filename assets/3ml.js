var node = document.getElementById('app');
var app = Elm.Main.embed(node, sessionStorage.session || null);

app.ports.getImgWidth.subscribe(function(selector) {
    getWidth(selector, 0, function (result) {
        app.ports.imgWidth.send(result);
    });
});

app.ports.printWindow.subscribe(function() {
    window.print();
});

app.ports.postProcessStory.subscribe(function (words) {
    renderStoryContent ("storycontent", words);
});

app.ports.storeSession.subscribe(function(session) {
    sessionStorage.session = session;
});

app.ports.checkPassword.subscribe(function(password) {
    var report = zxcvbn(password);
    app.ports.passwordChecked.send(report);
});

var getWidth = function (selector, count, callback) {
    var node = document.querySelector(selector);

    if (count > 10) {
        return callback(0);
    }

    if (!nullOrEmpty(node)) {
        return callback(node.naturalWidth);
    }

    setTimeout(function () {
        getWidth(selector, count + 1, callback);
    }, 5);

    return callback(0);
};

var nullOrEmpty = function (n) {
    return n === null || n === undefined || n.naturalWidth === 0;
};

var renderStoryContent = function (elt, words) {
    var wordIndex = function(word) {
        for (var i=0; i < words.length; i++) {
            var w = words[i];
            if (w.word === word) {
                return w.index;
            }
        }
        return -1;
    };

    var decorateDictWords = function (elt) {
        var ems = elt.querySelectorAll('p em');

        for (var i=0; i < ems.length; i++) {
            var em = ems[i];
            var w = em.innerText;
            var ix = wordIndex(w);
            if (ix < 0) {
                continue;
            }
            em.innerHTML = '<span class="dict-lookup" data-index="' + ix + '">' + em.innerText + '</span>';
            em.addEventListener('click', function () {
                var span = this.querySelector('span');
                var index = span.getAttribute('data-index');
                app.ports.dictLookup.send([span.innerText, parseInt(index)]);
            });
        }
    };

    var decorateExternalLinks = function (elt) {
        var links = elt.querySelectorAll('a');

        for (var i=0; i < links.length; i++) {
            var a = links[i];
            if (a.host !== location.host) {
                a.target = '_blank';
            }
        }
    };

    var go = function () {
        var storyElt = document.getElementById(elt);

        if (!storyElt) {
            window.requestAnimationFrame(go);
        } else {
            decorateDictWords(storyElt);
            decorateExternalLinks(storyElt);
        }
    };
    go();
};
