document.addEventListener('DOMContentLoaded', () => {
    const form = document.getElementById('crimeForm');
    const crimeTypeSelect = document.getElementById('crimeType');
    const victimsSection = document.getElementById('victimsSection');
    const addVictimBtn = document.getElementById('addVictim');
    const suspectsSection = document.getElementById('suspectsSection');
    const addSuspectBtn = document.getElementById('addSuspect');

    // Affiche/masque les champs spécifiques selon le type de crime
    function toggleCrimeSpecificFields() {
        const crimeType = crimeTypeSelect.value;

        document.querySelectorAll('.death-time').forEach(el => {
            const input = el.querySelector('.victim-death-time');
            if (crimeType === 'meurtre') {
                el.style.display = 'block';
                input.required = true;
            } else {
                el.style.display = 'none';
                input.required = false;
            }
        });

        document.querySelectorAll('.victim-fraud').forEach(el => {
            const loss = el.querySelector('.victim-loss');
            const method = el.querySelector('.victim-method');
            if (crimeType === 'escroquerie') {
                el.style.display = 'block';
                loss.required = true;
                method.required = true;
            } else {
                el.style.display = 'none';
                loss.required = false;
                method.required = false;
            }
        });
    }

    crimeTypeSelect.addEventListener('change', toggleCrimeSpecificFields);

    // Ajouter une victime
    addVictimBtn.addEventListener('click', () => {
        const newVictim = document.createElement('div');
        newVictim.classList.add('victim-input');
        newVictim.innerHTML = `
            <input type="text" class="victim-name" placeholder="Nom de la victime" required>
            <select class="victim-sex" required>
                <option value="">Sexe...</option>
                <option value="homme">Homme</option>
                <option value="femme">Femme</option>
                <option value="autre">Autre</option>
            </select>
            <input type="number" class="victim-age" placeholder="Âge" min="0" required>
            <select class="victim-marital" required>
                <option value="">Statut marital...</option>
                <option value="marie">Marié</option>
                <option value="celibataire">Célibataire</option>
            </select>
            <div class="death-time" style="display:none; grid-column: span 2;">
                <input type="time" class="victim-death-time" placeholder="Heure du décès">
            </div>
            <div class="victim-fraud" style="display:none; grid-column: span 2;">
                <input type="number" class="victim-loss" placeholder="Montant escroqué (€)">
                <select class="victim-method" required>
                    <option value="">Mode opératoire...</option>
                    <option value="phishing">Phishing</option>
                    <option value="arnaque_telephone">Arnaque par téléphone</option>
                    <option value="faux_site">Faux site web</option>
                    <option value="cheque_frauduleux">Chèque frauduleux</option>
                    <option value="autre">Autre</option>
                </select>
            </div>
            <button type="button" class="remove">🗑️ Supprimer</button>
        `;
        victimsSection.insertBefore(newVictim, addVictimBtn);
        attachRemoveListeners();
        toggleCrimeSpecificFields();
    });

    // Ajouter un suspect
    addSuspectBtn.addEventListener('click', () => {
        const newSuspect = document.createElement('div');
        newSuspect.classList.add('suspect-input');
        newSuspect.innerHTML = `
            <input type="text" class="suspect-name" placeholder="Nom du suspect" required>
            <input type="number" class="suspect-age" placeholder="Âge" min="0" required>
            <select class="suspect-sex" required>
                <option value="">Sexe...</option>
                <option value="homme">Homme</option>
                <option value="femme">Femme</option>
                <option value="autre">Autre</option>
            </select>
            <select class="suspect-relation" required>
                <option value="">Relation avec la victime...</option>
                <option value="ami">Ami</option>
                <option value="employeur">Employeur</option>
                <option value="dette">Dette</option>
                <option value="employee">Employé</option>
            </select>
            <select class="suspect-distance" required>
                <option value="">Distance...</option>
                <option value="pret">Près</option>
                <option value="loin">Loin</option>
            </select>
            <select class="suspect-arme" required>
                <option value="">Possède une arme...</option>
                <option value="oui">Oui</option>
                <option value="non">Non</option>
            </select>
            <select class="suspect-empreinte-lieu" required>
                <option value="">Empreinte sur le lieu...</option>
                <option value="oui">Oui</option>
                <option value="non">Non</option>
            </select>
            <select class="suspect-empreinte-arme" required>
                <option value="">Empreinte sur l'arme...</option>
                <option value="oui">Oui</option>
                <option value="non">Non</option>
            </select>
            <button type="button" class="remove">🗑️ Supprimer</button>
        `;
        suspectsSection.insertBefore(newSuspect, addSuspectBtn);
        attachRemoveListeners();
    });

    // Supprimer victime ou suspect
    function attachRemoveListeners() {
        document.querySelectorAll('.remove').forEach(btn => {
            btn.onclick = null;
            btn.addEventListener('click', e => {
                e.target.closest('.victim-input, .suspect-input').remove();
            });
        });
    }

    // Soumission formulaire
    form.addEventListener('submit', e => {
        e.preventDefault();
        const crimeType = crimeTypeSelect.value;

        const victims = Array.from(document.querySelectorAll('.victim-input')).map(div => {
            const name = div.querySelector('.victim-name').value.trim();
            if (!name) return null;
            const sex = div.querySelector('.victim-sex').value;
            const age = parseInt(div.querySelector('.victim-age').value) || 0;
            const marital = div.querySelector('.victim-marital').value;
            const deathTime = crimeType === 'meurtre' ? div.querySelector('.victim-death-time').value : null;
            const loss = crimeType === 'escroquerie' ? parseFloat(div.querySelector('.victim-loss').value) || 0 : null;
            const method = crimeType === 'escroquerie' ? div.querySelector('.victim-method').value : null;
            return { name, sex, age, marital, deathTime, loss, method };
        }).filter(v => v !== null);

        const suspects = Array.from(document.querySelectorAll('.suspect-input')).map(div => {
            const name = div.querySelector('.suspect-name').value.trim();
            if (!name) return null;
            const age = parseInt(div.querySelector('.suspect-age').value) || 0;
            const sex = div.querySelector('.suspect-sex').value;
            const relation = div.querySelector('.suspect-relation').value;
            const distance = div.querySelector('.suspect-distance').value;
            const arme = div.querySelector('.suspect-arme').value;
            const empreinteLieu = div.querySelector('.suspect-empreinte-lieu').value;
            const empreinteArme = div.querySelector('.suspect-empreinte-arme').value;
            return { name, age, sex, relation, distance, arme, empreinteLieu, empreinteArme };
        }).filter(s => s !== null);

        if (!crimeType || victims.length === 0 || suspects.length === 0) {
            alert('Veuillez remplir tous les champs obligatoires.');
            return;
        }

        const data = { crimeType, victims, suspects };

        fetch('http://localhost:8182/enquete', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(data)
        })
        .then(res => res.ok ? res.json() : Promise.reject(`Erreur HTTP: ${res.status}`))
        .then(result => showModal(result))
        .catch(err => showModal({ error: err }));
    });

    function showModal(result) {
        const modal = document.getElementById('resultModal');
        const modalResult = document.getElementById('modalResult');
        modalResult.innerHTML = '';

        if (result.error) {
            modalResult.innerHTML = `<div class="error"><h3>❌ Erreur</h3><p>${result.error}</p></div>`;
        } else {
            let html = '';

            if (result.crimeType) {
                html += `<div class="result-section"><h3>📌 Type de Crime</h3><p><strong>${result.crimeType}</strong></p></div>`;
            }

            if (result.victims && result.victims.length) {
                html += `<div class="result-section"><h3>👤 Victimes</h3><ul>`;
                result.victims.forEach(v => {
                    html += `<li>
                        <strong>${v.name}</strong> (${v.age} ans, ${v.sex}, ${v.marital})
                        ${v.deathTime ? `<br>⏰ Heure du décès : ${v.deathTime}` : ''}
                        ${v.loss ? `<br>💰 Montant escroqué : ${v.loss} €` : ''}
                        ${v.method ? `<br>📝 Mode opératoire : ${v.method}` : ''}
                    </li>`;
                });
                html += `</ul></div>`;
            }

            if (result.results && result.results.length) {
                html += `<div class="result-section"><h3>🕵️‍♂️ Analyse des Suspects</h3><ul>`;
                result.results.forEach(s => {
                    const badge = s.isGuilty 
                        ? '<span class="badge guilty">Coupable 🔥</span>' 
                        : '<span class="badge innocent">Innocent ✅</span>';
            
                    html += `<li>
                        <strong>${s.name}</strong> (${s.age} ans, ${s.sex}) ${badge}<br>
                        🔗 Relation : ${s.relation} | 📍 Distance : ${s.distance}<br>`;
            
                    if (result.crimeType === 'meurtre') {
                        html += `🔫 Arme : ${s.arme} | 🖐️ Empreinte lieu : ${s.empreinteLieu} | 🗡️ Empreinte arme : ${s.empreinteArme}<br>`;
                    } else if (result.crimeType === 'escroquerie') {
                        html += `💰 Montant : ${s.montant || 'N/A'} | 🎭 Mode opératoire : ${s.modeOperatoire || 'N/A'}<br>`;
                    } else if (result.crimeType === 'vol') {
                        html += ` 🖐️ Empreinte lieu : ${s.empreinteLieu} | 🔫 Arme : ${s.arme} | 🗡️ Empreinte arme : ${s.empreinteArme}<br>`;
                    }else if (result.crimeType === 'viol') {
                        html += ` 🖐️ Empreinte lieu : ${s.empreinteLieu} | 🔫 Arme : ${s.arme} | 🗡️ Empreinte arme : ${s.empreinteArme}<br>`;
                    }
                    
            
                    html += `📊 Probabilité d'être coupable: ${s.probabilite}%</li>`;
                });
                html += `</ul></div>`;
            }
            

            if (!html) html = '<p>Aucun résultat trouvé.</p>';
            modalResult.innerHTML = html;
        }

        modal.style.display = 'flex';
        document.getElementById('closeModal').onclick = () => modal.style.display = 'none';
        window.onclick = e => { if (e.target === modal) modal.style.display = 'none'; };

        // Reset formulaire
        form.reset();
        document.querySelectorAll('.victim-input').forEach((v,i) => i!==0 && v.remove());
        document.querySelectorAll('.suspect-input').forEach((s,i) => i!==0 && s.remove());
        toggleCrimeSpecificFields();
    }

    attachRemoveListeners();
    toggleCrimeSpecificFields();
});
