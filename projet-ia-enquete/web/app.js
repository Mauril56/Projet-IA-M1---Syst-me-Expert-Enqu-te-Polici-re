// Application web pour le système d'enquête IA
document.addEventListener('DOMContentLoaded', function() {
    console.log('🔍 Système d\'enquête IA initialisé');
    initializeApp();
});

function initializeApp() {
    loadSuspects();
    loadCrimes();
    setupEventListeners();
}

function loadSuspects() {
    const suspects = [
        {id: 'john', name: 'John'},
        {id: 'mary', name: 'Mary'}, 
        {id: 'alice', name: 'Alice'},
        {id: 'bruno', name: 'Bruno'},
        {id: 'sophie', name: 'Sophie'}
    ];
    
    const select = document.getElementById('suspectSelect');
    
    suspects.forEach(suspect => {
        const option = document.createElement('option');
        option.value = suspect.id;
        option.textContent = suspect.name;
        select.appendChild(option);
    });
}

function loadCrimes() {
    const crimes = [
        {id: 'vol', name: 'Vol'},
        {id: 'assassinat', name: 'Assassinat'},
        {id: 'escroquerie', name: 'Escroquerie'}
    ];
    
    const select = document.getElementById('crimeSelect');
    
    crimes.forEach(crime => {
        const option = document.createElement('option');
        option.value = crime.id;
        option.textContent = crime.name;
        select.appendChild(option);
    });
}

function setupEventListeners() {
    document.getElementById('analyzeBtn').addEventListener('click', analyzeSuspect);
    document.getElementById('hypothesisBtn').addEventListener('click', runHypothesis);
    document.getElementById('proofTreeBtn').addEventListener('click', showProofTree);
}

function analyzeSuspect() {
    const suspect = document.getElementById('suspectSelect').value;
    const crime = document.getElementById('crimeSelect').value;
    const resultDiv = document.getElementById('result');
    
    if (!suspect || !crime) {
        showWarning(resultDiv, 'Veuillez sélectionner un suspect et un crime');
        return;
    }
    
    // Simulation d'analyse (en production, appeler l'API Prolog)
    simulateAnalysis(suspect, crime, resultDiv);
}

function runHypothesis() {
    const suspect = document.getElementById('suspectSelect').value;
    const crime = document.getElementById('crimeSelect').value;
    const proofType = document.getElementById('proofSelect').value;
    const resultDiv = document.getElementById('hypothesisResult');
    
    if (!suspect || !crime || !proofType) {
        showWarning(resultDiv, 'Veuillez compléter tous les champs');
        return;
    }
    
    // Simulation de scénario hypothétique
    simulateHypothesis(suspect, crime, proofType, resultDiv);
}

function showProofTree() {
    const suspect = document.getElementById('suspectSelect').value;
    const crime = document.getElementById('crimeSelect').value;
    const treeDiv = document.getElementById('proofTree');
    
    if (!suspect || !crime) {
        showWarning(treeDiv, 'Veuillez sélectionner un suspect et un crime');
        return;
    }
    
    // Simulation d'arbre de preuves
    simulateProofTree(suspect, crime, treeDiv);
}

function simulateAnalysis(suspect, crime, resultDiv) {
    // Données de test prédéfinies
    const testResults = {
        'john_vol': {guilty: true, certainty: 85},
        'mary_assassinat': {guilty: true, certainty: 92},
        'alice_escroquerie': {guilty: true, certainty: 78},
        'bruno_vol': {guilty: false, certainty: 15},
        'sophie_escroquerie': {guilty: true, certainty: 65}
    };
    
    const key = `${suspect}_${crime}`;
    const result = testResults[key] || {
        guilty: Math.random() > 0.5,
        certainty: Math.floor(Math.random() * 100)
    };
    
    const analysis = generateAnalysis(suspect, crime, result.guilty);
    
    resultDiv.innerHTML = `
        <div class="result-card ${result.guilty ? 'guilty' : 'innocent'}">
            <h3>🔍 Résultat de l'analyse</h3>
            <p><strong>Suspect:</strong> ${suspect}</p>
            <p><strong>Crime:</strong> ${crime}</p>
            <p><strong>Verdict:</strong> ${result.guilty ? '✅ COUPABLE' : '✅ INNOCENT'}</p>
            <p><strong>Certitude IA:</strong> ${result.certainty}%</p>
            <p><strong>Analyse détaillée:</strong> ${analysis}</p>
            <p><strong>Heure:</strong> ${new Date().toLocaleTimeString()}</p>
        </div>
    `;
}

function simulateHypothesis(suspect, crime, proofType, resultDiv) {
    const impact = Math.floor(Math.random() * 40) - 15;
    const currentCertainty = Math.floor(Math.random() * 100);
    const newCertainty = Math.max(0, Math.min(100, currentCertainty + impact));
    
    const proofNames = {
        'motive': 'Motif criminel',
        'presence': 'Présence sur les lieux',
        'fingerprints': 'Empreintes digitales',
        'witness': 'Témoin oculaire',
        'transaction': 'Transaction bancaire',
        'fake_id': 'Fausse identité'
    };
    
    resultDiv.innerHTML = `
        <div class="result-card">
            <h3>🤔 Scénario Hypothétique</h3>
            <p><strong>Preuve ajoutée:</strong> ${proofNames[proofType]}</p>
            <p><strong>Impact sur la certitude:</strong> <span style="color: ${impact >= 0 ? '#27ae60' : '#e74c3c'}">${impact >= 0 ? '+' : ''}${impact}%</span></p>
            <p><strong>Nouvelle certitude:</strong> ${newCertainty}%</p>
            <p><strong>Interprétation:</strong> ${impact >= 0 ? 'Renforce la suspicion' : 'Affaiblit la suspicion'}</p>
        </div>
    `;
}

function simulateProofTree(suspect, crime, treeDiv) {
    const proofs = [
        {name: 'Motif criminel', present: Math.random() > 0.3},
        {name: 'Présence sur les lieux', present: Math.random() > 0.4},
        {name: 'Empreintes digitales', present: Math.random() > 0.5},
        {name: 'Témoin oculaire', present: Math.random() > 0.6},
        {name: 'Transaction bancaire', present: crime === 'escroquerie' ? Math.random() > 0.2 : Math.random() > 0.7},
        {name: 'Fausse identité', present: crime === 'escroquerie' ? Math.random() > 0.3 : Math.random() > 0.8}
    ];
    
    let html = `
        <div class="proof-tree">
            <h3>🌳 Arbre de preuves pour ${suspect} (${crime})</h3>
            <ul>
    `;
    
    proofs.forEach(proof => {
        html += `
            <li>${proof.name}: ${proof.present ? '✅ Présent' : '❌ Absent'}</li>
        `;
    });
    
    html += `
            </ul>
            <p><strong>Total des preuves:</strong> ${proofs.filter(p => p.present).length}/${proofs.length}</p>
        </div>
    `;
    
    treeDiv.innerHTML = html;
}

function generateAnalysis(suspect, crime, isGuilty) {
    const analyses = {
        guilty: [
            `L'analyse convergente des preuves matérielles et comportementales établit une forte probabilité de culpabilité.`,
            `Le système IA a détecté un pattern correspondant aux cas résolus précédents avec une haute fiabilité.`,
            `Plusieurs éléments de preuve corroborent la thèse de la culpabilité, formant une chaîne probante solide.`
        ],
        innocent: [
            `L'absence de preuves matérielles concluantes et la cohérence des alibis militent en faveur de l'innocence.`,
            `Le système IA n'a pas détecté de pattern correspondant aux cas de culpabilité avérée.`,
            `Les incohérences dans les accusations et l'absence de mobile credible suggèrent une erreur judiciaire potentielle.`
        ]
    };
    
    const type = isGuilty ? 'guilty' : 'innocent';
    return analyses[type][Math.floor(Math.random() * analyses[type].length)];
}

function showWarning(element, message) {
    element.innerHTML = `
        <div class="alert alert-warning">
            ⚠️ ${message}
        </div>
    `;
}

function resetAll() {
    if (confirm('Êtes-vous sûr de vouloir tout réinitialiser ?')) {
        document.getElementById('suspectSelect').value = '';
        document.getElementById('crimeSelect').value = '';
        document.getElementById('proofSelect').value = '';
        document.getElementById('result').innerHTML = '';
        document.getElementById('hypothesisResult').innerHTML = '';
        document.getElementById('proofTree').innerHTML = '';
        console.log('🔄 Interface réinitialisée');
    }
}

// Exposer les fonctions globalement pour le HTML
window.resetAll = resetAll;