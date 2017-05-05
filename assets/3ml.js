var node = document.getElementById('app');
var app = Elm.Main.embed(node);

app.ports.getImgWidth.subscribe(function(selector) {
    getWidth(selector, 0, function (result) {
        app.ports.imgWidth.send(result);
    });
});

app.ports.printWindow.subscribe(function() {
    window.print();
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
};

var nullOrEmpty = function (n) {
    return n === null || n === undefined || n.naturalWidth === 0;
};
