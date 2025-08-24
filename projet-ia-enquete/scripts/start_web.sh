#!/bin/bash
echo "🌐 Lancement de l'interface Web..."
echo "================================="

# Ouvrir l'interface web dans le navigateur par défaut
if command -v xdg-open > /dev/null; then
    xdg-open web/index.html
elif command -v open > /dev/null; then
    open web/index.html
else
    echo "Ouvrez manualement: web/index.html"
fi

echo "Interface web prête!"